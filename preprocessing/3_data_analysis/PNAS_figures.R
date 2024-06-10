library(tidyverse)
library(readxl)
library(ggrepel)
library(viridis)
library(geomtextpath)
library(countrycode)
library(gridExtra)
library(stats)
library(ggbump)
library(GGally)
library(openxlsx)
library(cowplot)
library(ggridges)
library(plotly)
library(patchwork)
library(wesanderson)
library(RColorBrewer)
options(scipen = 9999)

####tables#####

paper_level_tables <- readRDS('results/paper_level_tables_1990.RDS')

latam_meta <- read_delim("data/latam/latam_meta_1990.csv", 
                         delim = ";", escape_double = FALSE, 
                         col_names = c('Pub_ID', 'pub_year','source_title', 'source_type',
                                       'level1', 'level2', 'n_cits'), 
                         trim_ws = TRUE
                         
)  %>% 
  select('Pub_ID','level1','level2', 'pub_year','n_cits') %>% 
  distinct(Pub_ID, .keep_all = TRUE) %>% 
  filter(!is.na(level2)) 

denominator <- latam_meta %>% 
  group_by(pub_year,level2) %>% 
  summarise(mean_citations = mean(n_cits))

latam_meta <- latam_meta %>% 
  left_join(.,denominator) %>% 
  mutate(normalized_citations = n_cits/mean_citations)

latam_meta<- latam_meta%>% 
  mutate(level1 = str_to_title(substr(level1, 3, nchar(level1))))%>% 
  mutate(level2 = str_to_title(substr(level2, 6, nchar(level2))))


latam_meta<- latam_meta %>% 
  filter(pub_year %in% c(1993:2022))

latam_country_codes <- c('AR','BO','BR','CL','CO','MX','PY','PE','UY','VE')

latam_authors <- readRDS("data/latam/latam_authors_1990.RDS")

aux_journal_country <- readRDS("results/aux_journal_country_1990.RDS")%>% 
  select(-journal_country) %>% 
  unique()

aux_conference_country <- readRDS("results/aux_conference_country_1990.RDS") %>% 
  select(-proc_country) %>% 
  unique() %>% 
  rename("latam_journal" = "latam_conf") %>% 
  janitor::clean_names()

journal_aux <- bind_rows(aux_journal_country,aux_conference_country) %>% unique()

latam_citations_1990 <- read_delim("data/latam/latam_citations_1990.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                   col_names = c("citing_pub_id","cited_pub_id","cit_window",'is_self_cit')) %>% select("citing_pub_id","cited_pub_id")

document_topics <- read_csv("data/topic_model/bertopic_output_20240302_195213/document_topics.csv")

topic_info <- read_csv("data/topic_model/bertopic_output_20240302_195213/topic_info.csv")

topic_labels <- topic_info %>% 
  janitor::clean_names() %>% 
  mutate(name = gsub("_"," ",name)) %>% 
  mutate(name = ifelse(topic <1000,
                       ifelse(topic <100, 
                              substr(name, 3, nchar(name)),
                              substr(name, 4, nchar(name))),
                       substr(name, 5, nchar(name)))
  ) %>% 
  mutate(name = str_squish(name)) %>% 
  mutate(label = paste(word(name,1),word(name,2)))

latam_meta$level1[latam_meta$level1 == "Medical And Health Sciences" ] <- "Medical and Health Sciences" 
latam_meta$level1[latam_meta$level1 == "Engineering And Technology" ] <- "Engineering and Technology" 

###figure 1####

fig1_table <- latam_meta %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id"))

ffig1a_table <- latam_meta %>% 
  inner_join(.,(paper_level_tables$paper_gender_dist),
             by="Pub_ID") %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id"))

ffig1b_table <- latam_meta %>% 
  left_join(.,(latam_authors),
            by="Pub_ID") %>% 
  filter(country_code%in%latam_country_codes) %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  filter(gender %in% c("Men","Women")) %>% 
  select(Pub_ID,pub_year,author_id,gender) %>% 
  unique()


test_size_fig1 <- 10

#####figure 1a#####

plot_a <- ffig1a_table%>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(pub_year) %>% 
  summarise(w_latam = sum(Women[latam_journal=="Latin American journal"]),
            w_n_latam = sum(Women[latam_journal!="Latin American journal"]),
            m_latam = sum(Men[latam_journal=="Latin American journal"]),
            m_n_latam = sum(Men[latam_journal!="Latin American journal"]))%>% 
  # summarise("In Latin America" = n_distinct(Pub_ID[latam_journal=="Latin American journal"]),
  #           "Outside Latin America"= n_distinct(Pub_ID[latam_journal!="Latin American journal"])
  pivot_longer(!pub_year,names_to = "ind", values_to = "value")  %>% 
  mutate(Circuit = case_when(ind %in% c("w_latam","m_latam") ~ "In Latin America",
                             TRUE ~"Outside Latin America"))  %>% 
  mutate(gender = case_when(ind %in% c("w_latam","w_n_latam") ~ "Women",
                            TRUE ~"Men"))  %>% 
  ggplot(.,aes(y=value,x=as.numeric(pub_year),group = ind,color = gender, linetype = Circuit ))+
  geom_line()+
  #geom_vline(aes(xintercept = 2003), size = .2, linetype = "longdash",alpha=.5)+
  #geom_vline(aes(xintercept = 2012), size = .2, linetype = "longdash",alpha=.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size =test_size_fig1))+
  scale_color_brewer(palette = "Paired",direction = 1)+
  labs(y = "Number of publications",
       x = "Publication year", fill = "", color = "", linetype = "",
       title= "A")

#yearly growth rate

ffig1a_table%>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(pub_year) %>% 
  summarise(w_n_latam = sum(Women[latam_journal!="Latin American journal"]),
            m_n_latam = sum(Men[latam_journal!="Latin American journal"])) |>
  mutate(w_growth = (w_n_latam - lag(w_n_latam))/w_n_latam,
         m_growth = (m_n_latam - lag(m_n_latam))/m_n_latam) |> 
  # pivot_longer(c("w_growth","m_growth"),names_to = "ind", values_to = "value")  %>% 
  # ggplot(aes(x = pub_year, y = value, color = ind, group = ind))+
  # geom_point()+
  # geom_line()
  summarise(w = mean(w_growth, na.rm = TRUE),
            m = mean(m_growth, na.rm = TRUE))
  
  

#####figure 1b#####

plot_b <- ffig1b_table%>% 
  group_by(pub_year) %>% 
  summarise(Women =n_distinct(author_id[gender=="Women"]),
            n = n_distinct(author_id)) %>% 
  ungroup() %>% 
  mutate("Women authors" = Women/n) %>% 
  select(-Women,-n) %>% 
  left_join(.,
            (ffig1a_table%>% 
               group_by(pub_year) %>% 
               summarise("Women authorship" =mean(Women)) )) %>% 
  pivot_longer(!pub_year, names_to = "ind", values_to = "value") %>% 
  
  ggplot(.,aes(y=value,x=as.numeric(pub_year),group = ind,color = ind, linetype = ind))+
  geom_line()+
  #geom_point()+
  #geom_vline(aes(xintercept = 2003), size = .2, linetype = "longdash",alpha=.5)+
  #geom_vline(aes(xintercept = 2012), size = .2, linetype = "longdash",alpha=.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size = test_size_fig1))+
  scale_color_viridis(discrete = TRUE,option = "F", begin = .2, end =.5)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(x = "Publication year", 
       y = "Women authors / \n Women authorship",
       color = "",  linetype = "",
       title = "B"
  )+
  #geom_hline(yintercept = .5, size=.1)+
  guides(fill = guide_legend(reverse=TRUE))


#####figure 1c#####


plot_c <- ffig1a_table%>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(pub_year) %>% 
  summarise(n =n_distinct(Pub_ID),
            n_journal = n_distinct(Pub_ID[latam_journal=="Latin American journal"]),
            n_w =sum(Women),
            n_journal_w = sum(Women[latam_journal=="Latin American journal"]),
            n_m =sum(Men),
            n_journal_m = sum(Men[latam_journal=="Latin American journal"])
            
  ) %>% 
  mutate("Total" = n_journal/n,
         "Women" = n_journal_w/n_w ,
         "Men" = n_journal_m/n_m ) %>% 
  pivot_longer(c("Total", "Women","Men"),names_to = "ind", values_to = "prop") |> 
  
  ggplot(aes(y=prop,x=as.numeric(pub_year),color = ind,group=ind ,linetype = ind ))+
  geom_line()+
  # geom_vline(aes(xintercept = 2003), size = .2, linetype = "longdash",alpha=.5)+
  #  geom_vline(aes(xintercept = 2012), size = .2, linetype = "longdash",alpha=.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size =test_size_fig1))+
  scale_color_manual(values = c("#A6CEE3", "black", "#1F78B4"))+
  scale_linetype_manual(values=c("dashed", "solid", "dashed")) +  
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(y = "Published in Latin American \n journals or conferences",
       x = "Publication year", color = "",linetype ="",
       title= "C")

#####figure 1d#####

plot_d <- ffig1b_table%>% 
  group_by(pub_year) %>% 
  summarise(Women =n_distinct(author_id[gender=="Women"]),
            n = n_distinct(author_id)) %>% 
  ungroup() %>% 
  mutate("Women authors" = Women/n) %>% 
  select(-Women,-n) %>% 
  left_join(.,
            (ffig1a_table%>% 
               group_by(pub_year) %>% 
               summarise("Women authorship" =mean(Women)) )) %>% 
  mutate(gap = (`Women authors`- `Women authorship`)/`Women authors`) %>% 
  ggplot(.,aes(y=gap,x=as.numeric(pub_year)))+
  geom_point(size=.2)+
  geom_smooth(color = "black", size=.5,fill = "lightgray")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size = test_size_fig1))+
  #scale_color_viridis(discrete = TRUE,option = "F", begin = .2, end =.5)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),limits = c(0.1,NA))+
  labs(x = "Publication year", 
       y = "Gap between women authors \n and women authorship",
       title = "D"
  )+
  guides(fill = guide_legend(reverse=TRUE))

#women and men productivity gap

# n_authors <- ffig1b_table%>%
#   group_by(pub_year) %>%
#   summarise(Women =n_distinct(author_id[gender=="Women"]),
#             Men =n_distinct(author_id[gender=="Men"])) %>%
#   ungroup() |>
#   pivot_longer(c("Women","Men"), names_to = "gender", values_to = "authors")

# n_pubs <- ffig1a_table%>%
#   group_by(pub_year) %>%
#   summarise(Women =sum(Women),
#             Men =sum(Men))|>
#   pivot_longer(c("Women","Men"), names_to = "gender", values_to = "pubs")
# 
# n_authors |>
#   left_join(n_pubs, by = c("pub_year","gender")) |>
#   mutate(productivity = pubs/authors) |>
#   select(-pubs,-authors) |>
#   pivot_wider(id_cols = "pub_year", names_from = "gender", values_from = "productivity") |>
#   mutate(gap = (Men - Women)/Men) |>
#   ggplot(aes(y=gap,x=as.numeric(pub_year),group = 1))+
#   geom_line()+
#   theme_minimal()

#####comp#####

plot_grid(plot_a, plot_b, plot_c, plot_d, ncol=2, rel_heights = c(1/2, 1/2))

ggsave('results/figures_PNAS/latam_context.png', width = 10, height = 8, dpi = 350,bg = "white")

####figure 2#####

base_table <- document_topics %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  left_join(.,(latam_meta %>% 
                 distinct(Pub_ID,level1,normalized_citations) )) %>% 
  left_join(paper_level_tables$paper_gender_dist) %>% 
  filter(Topic != -1) %>% 
  rename("topic" = "Topic")

order <- base_table %>% 
  filter(!is.na(level1))  %>% 
  group_by(level1) %>% 
  summarise(n = n_distinct(Pub_ID[latam_journal=="Latin American journal"])/n_distinct(Pub_ID[!is.na(latam_journal)])) %>% 
  arrange(-n) %>% 
  mutate(order = row_number()) %>% 
  select(-n)

#####figure 2a#####

plot_a <- base_table %>% 
  filter(!is.na(level1)) |> 
  filter(!is.na(latam_journal)) %>% 
  group_by(latam_journal,level1) %>% 
  summarise(n = n_distinct(Pub_ID)) %>% 
  group_by(level1) %>% 
  mutate(prop = n/sum(n)) %>% 
  select(-n) %>% 
  left_join(.,order) %>% 
  mutate(latam_journal = case_when(latam_journal == "Latin American journal" ~"Published in Latin America",
                                   latam_journal == "Non Latin American journal" ~"Not published in Latin America")) %>% 
  ggplot(.,aes(x = fct_reorder(level1,order), y = prop, fill = latam_journal))+
  geom_col(position = "stack")+
  theme_minimal()+
  theme(text = element_text(size = 12))+
  scale_fill_brewer(palette = "Dark2",direction = 1)+
  theme(legend.position = "top")+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(fill = "", x = "", y = "Publications in each circuit",
       title = "A")+
  coord_flip()  +
  theme(plot.title = element_text( margin = margin(0,0,-10,0)))+
  scale_x_discrete(labels=function(x) str_wrap(x,10))

#####figure 2b#####

gender_distribution <- base_table %>% 
  left_join(.,latam_authors) %>% 
  filter(country_code%in%latam_country_codes) %>% 
  group_by(level1,gender) %>% 
  summarize(n = n_distinct(author_id)) %>% 
  filter(gender %in% c("Men","Women")) %>% 
  group_by(level1) %>% 
  mutate(prop = n/sum(n))

plot_b <- gender_distribution %>%
  filter(!is.na(level1)) |>
  select(-n) %>%
  pivot_wider(.,id_cols = "level1", names_from = "gender", values_from = "prop") %>%
  mutate(ratio = Men/Women) %>%
  left_join(.,order) %>%
  ggplot(.,aes(x = fct_reorder(level1,order), y = ratio))+
  geom_col(fill = "#7570B3")+
  theme_minimal()+
  theme(text = element_text(size = 12))+
  scale_fill_brewer(palette = "Paired",direction = 1)+
  theme(legend.position = "bottom")+
  #scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(fill = "", x = "", y = "Men to women authors ratio",
       title = "B", subtitle = "\n")+
  coord_flip() +
  theme(plot.title = element_text( margin = margin(0,0,-10,0)))+
  scale_x_discrete(labels=function(x) str_wrap(x,10))

# plot_b <- gender_distribution %>%
#   filter(!is.na(level1)) %>%
#   left_join(.,order) %>%
#   ggplot(.,aes(x = fct_reorder(level1,order), y = prop, fill = gender))+
#   geom_col(position = "stack")+
#   theme_minimal()+
#   theme(text = element_text(size = 12))+
#   scale_fill_brewer(palette = "Paired",direction = 1)+
#   theme(legend.position = "top")+
#   scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
#   labs(fill = "", x = "", y = "Authors' gender",
#        title = "B")+
#   coord_flip()

#####figure 2c#####

exp_1 <- paper_level_tables$paper_gender_dist %>% 
  left_join(.,latam_meta, by = "Pub_ID") %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  filter(!is.na(level1)) %>% 
  filter(!is.na(level2)) %>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(level2) %>% 
  summarise(n =n_distinct(Pub_ID),
            n_journal = n_distinct(Pub_ID[latam_journal=="Latin American journal"])) %>% 
  ungroup() %>% 
  mutate(p_journal = n_journal/n
  ) %>% 
  select(-n,-n_journal)

#gender dist by discipline
exp_2 <- paper_level_tables$paper_gender_dist %>% 
  left_join(.,latam_meta, by = "Pub_ID") %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  filter(!is.na(latam_journal)) %>% 
  filter(!is.na(level1)) %>% 
  filter(!is.na(level2)) %>% 
  group_by(level2) %>% 
  summarise(Men =sum(Men),
            Women =sum(Women)) %>% 
  pivot_longer(.,!c("level2"), names_to = "gender", values_to = "n") %>% 
  group_by(gender) %>% 
  mutate(n = n/sum(n))

exp_3 <-  exp_2 %>% 
  left_join(.,exp_1) %>% 
  mutate(expected = n*p_journal) %>% 
  group_by(gender) %>% 
  summarize(exp = sum(expected))

real_1 <- paper_level_tables$paper_gender_dist %>% 
  left_join(.,latam_meta, by = "Pub_ID") %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  filter(!is.na(level1)) %>% 
  filter(!is.na(level2)) %>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(#period,
           latam_journal) %>% 
  summarise(Men =sum(Men),
            Women =sum(Women)) %>% 
  pivot_longer(.,!c(#"period",
                    "latam_journal"), names_to = "gender", values_to = "n") %>% 
  group_by(#period,
           gender) %>% 
  mutate(n = n/sum(n)) %>% 
  filter(latam_journal == "Latin American journal") %>% 
  select(-latam_journal) %>% 
  rename("real" = "n")

plot_c <- real_1 %>% 
  left_join(.,exp_3) %>% 
  mutate(rel = real/exp -1) %>% 
  rename("Real" = "real",
         "Expected" = "exp",
         "Real/Expected" = "rel")  %>% 
  ggplot(.,aes(x=gender,y= `Real/Expected`,fill =gender))+
  geom_col(position = "dodge")+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(text = element_text(size = 12),
        axis.title.x = element_text(size = 11))+
  scale_fill_brewer(palette = "Paired",direction = 1)+
  labs(y = "Ratio between observed and expected \n % of publications in Latin America",
       x = "",
       title = "C")+
  geom_hline(yintercept = 0,size=.5)+
  coord_flip()

#####comp#####

plot_grid(plot_a, plot_b, plot_c, ncol=3,rel_widths = c(.4, .4,.3))
#grid.arrange(plot_a, plot_b, plot_c,  ncol=2)

ggsave('results/figures_PNAS/disc_circ_gend.png', width = 12, height = 4, dpi = 350,bg = "white")

####figure 3#####

#top 20
topic_label_scatter <- read_excel("preprocessing/3_data_analysis/topic_label_scatter.xlsx") %>% 
  select(topic,hand_label)

#bottom 20
topic_label_scatter_b <- read_excel("preprocessing/3_data_analysis/topic_label_scatter_b.xlsx") %>% 
  select(topic,hand_label)

topic_label_scatter <-  bind_rows(topic_label_scatter,topic_label_scatter_b)

fig_3_table <- document_topics %>% 
  inner_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  inner_join(.,(latam_meta %>% 
                  distinct(Pub_ID,level1) %>% 
                  filter(!is.na(level1)))) %>% 
  inner_join(paper_level_tables$paper_gender_dist) %>% 
  filter(Topic != -1) 

interdis_correction <- fig_3_table %>%
  group_by(Topic,level1) %>%
  summarise(n = n_distinct(Pub_ID)) %>%
  group_by(Topic) %>%
  mutate(n = n/sum(n)) %>%
  group_by(Topic) %>%
  slice_max(n=1,order_by=n) %>%
  mutate(level1 = ifelse(n>0.5,paste0(level1), paste0("Multidisciplinary"))) %>%
  select(-n) %>%
  #in case there is a 50-50
  unique()

fig_3_table <- fig_3_table %>%
  select(-level1) %>%
  left_join(.,interdis_correction)

fig_3_table <- fig_3_table %>%
  rename("topic" = "Topic")

summary <- fig_3_table %>% 
  filter(topic != -1) %>% 
  filter(!is.na(level1)) %>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(level1,topic) %>% 
  summarise(n_journal = n_distinct(Pub_ID[latam_journal=="Latin American journal"]),
            n = n_distinct(Pub_ID),
            women = mean(Women)) %>% 
  ungroup() %>% 
  mutate(latam = n_journal/n) %>% 
  select(-n_journal) %>% 
  left_join(topic_labels,by="topic") %>% 
  filter(n>50)


order <- summary %>% 
  group_by(level1) %>% 
  summarize(w = mean(women)) %>% 
  arrange(-w) %>% 
  mutate(order = row_number()) %>% 
  select(-w)

threshold <- 20

labeling_info <- summary %>% 
  arrange(women) %>% 
  mutate(women_desc =row_number()) %>% 
  arrange(-women) %>% 
  mutate(women_asc =row_number()) %>% 
  arrange(latam) %>% 
  mutate(latam_desc =row_number()) %>% 
  arrange(-latam) %>% 
  mutate(latam_asc =row_number()) %>% 
  mutate(labeled = ifelse((women_desc<threshold|
    women_asc<threshold|
      latam_desc<threshold|
      latam_asc<threshold)
    ,1,0
  )) %>% 
  left_join(.,topic_label_scatter) %>% 
  select(topic,labeled,hand_label) %>% 
  mutate(my_label = ifelse(labeled == 1,
                           paste(hand_label), ""))

# select(topic,label,labeled) %>% 
# mutate(my_label = ifelse(labeled == 1,
#                          paste(label), ""))


m_w <- median(summary$women)
m_l <- median(summary$latam)

mycolors = c("Social Sciences" = "#33A02C", 
             "Humanities" = "#B2DF8A", 
             "Medical and Health Sciences" = "#E31A1C", 
             "Multidisciplinary" = "#FDBF6F", 
             "Agricultural Sciences" = "#6A3D9A", 
             "Natural Sciences" = "#1F78B4", 
             "Engineering and Technology" = "#A6CEE3" )

#brewer.pal(n = 20, "Paired")
#  "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6"
# "#6A3D9A" "#FFFF99" "#B15928"

summary$level1[summary$level1 == "Medical And Health Sciences" ] <- "Medical and Health Sciences" 
summary$level1[summary$level1 == "Engineering And Technology" ] <- "Engineering and Technology" 

order$level1[order$level1 == "Medical And Health Sciences" ] <- "Medical and Health Sciences" 
order$level1[order$level1 == "Engineering And Technology" ] <- "Engineering and Technology" 

summary %>% 
  left_join(.,order) %>% 
  left_join(.,labeling_info) %>% 
  ggplot(.,aes(x=women,y=latam,size=count,color = fct_reorder(level1,order)))+
  geom_point(alpha=0.5)+
  geom_text_repel(aes(label = my_label),size= 2.5, alpha = 1,max.overlaps = 20)+
  geom_hline(yintercept = m_l)+
  geom_vline(xintercept = m_w)+
  #scale_color_viridis(discrete = T)+
  #scale_color_brewer(palette = "Paired",direction = -1)+
  scale_color_manual(values=mycolors)+
  scale_size(range = c(2, 10))+
  theme_minimal()+
  #theme(text = element_text(size = 15))+
  theme(text = element_text(size = 12))+
  scale_x_continuous(labels = function(x) paste0(x*100,"%"))+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(y="Published in Latin American journals or conferences",
       x="Women authorship",
       color="Discipline")+
  guides(size = "none")+
  theme(legend.position = c(0.15, 0.80))

ggsave('results/figures_PNAS/topic_scatter.png', width = 11, height = 6, dpi = 350,bg = "white")

#####Hand labels #######
labeling_info_hand <- summary %>% 
  arrange(women) %>% 
  mutate(women_desc =row_number()) %>% 
  arrange(-women) %>% 
  mutate(women_asc =row_number()) %>% 
  arrange(latam) %>% 
  mutate(latam_desc =row_number()) %>% 
  arrange(-latam) %>% 
  mutate(latam_asc =row_number()) %>% 
  mutate(labeled = ifelse((
    women_asc<threshold|
      latam_asc<threshold)
    ,1,0
  )) %>% 
  mutate(my_label = ifelse(labeled == 1,
                           paste(label), "")) %>% 
  filter(labeled ==1) %>% 
  select(topic,name,count,representation,representative_docs,labeled)

#write.xlsx(labeling_info_hand,"preprocessing/3_data_analysis/topic_table_scatter.xlsx")


labeling_info_hand <- summary %>% 
  arrange(women) %>% 
  mutate(women_desc =row_number()) %>% 
  arrange(-women) %>% 
  mutate(women_asc =row_number()) %>% 
  arrange(latam) %>% 
  mutate(latam_desc =row_number()) %>% 
  arrange(-latam) %>% 
  mutate(latam_asc =row_number()) %>% 
  mutate(labeled = ifelse((
    women_desc<threshold|
      latam_desc<threshold)
    ,1,0
  )) %>% 
  mutate(my_label = ifelse(labeled == 1,
                           paste(label), "")) %>% 
  filter(labeled ==1) %>% 
  select(topic,name,count,representation,representative_docs,labeled)

#write.xlsx(labeling_info_hand,"preprocessing/3_data_analysis/topic_table_scatter_b.xlsx")


####figure 4#####

order <- base_table %>% 
  filter(!is.na(level1))  %>% 
  group_by(level1) %>% 
  summarise(n = n_distinct(Pub_ID[latam_journal=="Latin American journal"])/n_distinct(Pub_ID[!is.na(latam_journal)])) %>% 
  arrange(-n) %>% 
  mutate(order = row_number()) %>% 
  select(-n)

#####figure 4A#####

topic_table <- base_table %>% 
  filter(!is.na(level1))  %>% 
  left_join(.,latam_meta) %>% 
  group_by(topic) %>% 
  summarize(latam_journal = n_distinct(Pub_ID[latam_journal=="Latin American journal"])/n_distinct(Pub_ID),
            women = mean(Women,na.rm = TRUE),
            citations = median(normalized_citations),
            raw_citations = median(n_cits),
            n = n_distinct(Pub_ID)) %>% 
  left_join(.,topic_labels)

topic_table <- topic_table %>% 
  filter(n>=30)

quants_topics <- topic_table %>% 
  mutate(quants_latam = case_when(latam_journal < mean(topic_table$latam_journal) ~ "Global topics",
                                  TRUE ~ "Regional topics")) 
table(quants_topics$quants_latam)

#quants_latam

plot_a <- quants_topics %>% 
  select(topic,citations,quants_latam) |> 
  pivot_wider(id_cols = "topic", values_from = "citations", names_from = "quants_latam") |> 
  ggplot(aes(x=x) ) +
  geom_density( aes(x = `Global topics`, y = ..density..), fill="#B2DF8A",alpha = .8 ) +
  annotate( "text",x=.5, y=.2, label="Global topics", color="black",size=3) +
  geom_density( aes(x = `Regional topics`, y = -..density..), fill= "#33A02C" ,alpha = .8 ) +
  annotate( "text",x=.5, y=-0.2, label="Regional topics", color="white",size=3) +
  theme(text= element_text(size = 12))+
  theme_minimal()+
  labs(x = "Average normalized citations",  title = "A")+
  theme(axis.text.y = element_blank()) 


#####figure 4B#####

full_table <- latam_citations_1990 %>% 
  mutate(latam_author = case_when(citing_pub_id %in% cited_pub_id ~ "yes",
                                  TRUE ~ "no")) %>% 
  left_join(.,journal_aux, by = c("cited_pub_id"= "pub_id"))%>% 
  left_join(.,(latam_meta %>% select('Pub_ID','level1')), by = c("cited_pub_id"="Pub_ID"))

summary_quant_origin <-full_table %>% 
  filter(!is.na(level1)) %>% 
  filter(!is.na(latam_journal)) %>% 
  left_join(.,(document_topics %>% rename("topic" = "Topic")), by = c("cited_pub_id" = "Pub_ID")) %>% 
  left_join(.,quants_topics, by= c("topic" = "topic")) 

gr_summary_quant_origin <- summary_quant_origin%>% 
  group_by(quants_latam,latam_author,topic) %>% 
  summarise(n = n_distinct(citing_pub_id)) %>% 
  group_by(quants_latam,topic) %>% 
  mutate(prop = n/sum(n)) 


plot_b <-  gr_summary_quant_origin %>% 
  filter(!is.na(quants_latam)) %>% 
  filter(latam_author == "yes") %>% 
  select(topic,prop,quants_latam) |> 
  pivot_wider(id_cols = "topic", values_from = "prop", names_from = "quants_latam") |> 
  ggplot(aes(x=x) ) +
  geom_density( aes(x = `Global topics`, y = ..density..), fill="#B2DF8A",alpha = .8 ) +
  annotate( "text",x=0.2, y=.5, label="Global topics", color="black",size=3) +
  geom_density( aes(x = `Regional topics`, y = -..density..), fill= "#33A02C" ,alpha = .8 ) +
  annotate( "text",x=0.2, y=-.5, label="Regional topics", color="white",size=3)+
  theme(text= element_text(size = 12))+
  theme_minimal()+
  scale_x_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(x = "% of Latin American citations",y="density", title = "B")+
  theme(axis.text.y = element_blank()) 


#####figure 4C#####

bootstrap_coefficients <-readRDS("results/bootstrap_confidence.RDS")
bootstrap_coefficients$level1[bootstrap_coefficients$level1 == "Medical And Health Sciences" ] <- "Medical and Health Sciences" 
bootstrap_coefficients$level1[bootstrap_coefficients$level1 == "Engineering And Technology" ] <- "Engineering and Technology" 

plot_c <- base_table %>% 
  filter(!is.na(level1)) %>% 
  filter(!is.na(latam_journal)) %>% 
  filter(!is.na(Women)) %>% 
  group_by(latam_journal,level1) %>% 
  summarise(Women = weighted.mean(normalized_citations,Women,na.rm = TRUE),
            Men = weighted.mean(normalized_citations,Men,na.rm = TRUE),
            n = n_distinct(Pub_ID)) %>% 
  ungroup() |> 
  pivot_longer(c("Women","Men"), names_to = "Gender", values_to = "citations") %>%
  left_join(bootstrap_coefficients, by = c("latam_journal","level1","Gender")) |> 
  
  mutate(latam_journal = case_when(latam_journal == "Latin American journal" ~"Published in Latin America",
                                   latam_journal == "Non Latin American journal" ~"Not published in Latin America")) %>% 
  left_join(.,order) %>% 
  mutate(group_legend = case_when(latam_journal == "Published in Latin America" & Gender == "Men" ~"Men - Published in Latin America",
                                  latam_journal == "Published in Latin America" & Gender == "Women" ~ "Women - Published in Latin America", 
                                  latam_journal == "Not published in Latin America" & Gender == "Men" ~ "Men - Not published in Latin America",
                                  latam_journal == "Not published in Latin America" & Gender == "Women" ~ "Women - Not published in Latin America"))  %>% 
  ggplot(.,aes(x = fct_reorder(level1,order), y = citations, shape = group_legend,color =group_legend,group = Gender))+
  geom_point(size = 3, alpha = 1,position=position_dodge(width =.2))+
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.3,alpha=.8,
    position=position_dodge(width=.2))+
  theme(text= element_text(size = 12))+
  theme_minimal()+
  scale_color_manual(values = c("#A6CEE3","#A6CEE3","#1F78B4","#1F78B4"))+
  labs(color = "", shape = "", 
       x = "", y = "Average normalized citations",
       title = "C")+
  scale_shape_manual( values = c(16,17,16,17))+
  scale_x_discrete(labels=function(x) str_wrap(x,15))+
  theme(legend.position = c(0.75, 0.85))



#####comp#####

(plot_a + plot_b) / plot_c + plot_layout(heights = c(3/8, 5/8))

ggsave('results/figures_PNAS/impact.png', width = 10, height = 10, dpi = 350,bg = "white")

