library(tidyverse)
library(readxl)
library(openxlsx)
options(scipen = 9999)

results_list <-  list()

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

#####figure 1a#####

# plot_1a_table <- latam_meta %>% 
#   inner_join(.,(paper_level_tables$paper_gender_dist),
#              by="Pub_ID") %>% 
#   left_join(.,journal_aux, by = c("Pub_ID" = "pub_id"))%>% 
#   filter(!is.na(latam_journal)) %>% 
#   group_by(pub_year) %>% 
#   summarise(w_latam = sum(Women[latam_journal=="Latin American journal"]),
#             w_n_latam = sum(Women[latam_journal!="Latin American journal"]),
#             m_latam = sum(Men[latam_journal=="Latin American journal"]),
#             m_n_latam = sum(Men[latam_journal!="Latin American journal"]))%>% 
#   # summarise("In Latin America" = n_distinct(Pub_ID[latam_journal=="Latin American journal"]),
#   #           "Outside Latin America"= n_distinct(Pub_ID[latam_journal!="Latin American journal"])
#   pivot_longer(!pub_year,names_to = "ind", values_to = "value")  %>% 
#   mutate(Circuit = case_when(ind %in% c("w_latam","m_latam") ~ "In Latin America",
#                              TRUE ~"Outside Latin America"))  %>% 
#   mutate(gender = case_when(ind %in% c("w_latam","w_n_latam") ~ "Women",
#                             TRUE ~"Men"))  


plot_1a_table <- latam_meta %>% 
  inner_join(.,(paper_level_tables$paper_gender_dist),
             by="Pub_ID") %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
filter(!is.na(latam_journal)) %>% 
  group_by(pub_year,level1) %>% 
  summarise(w_latam = sum(Women[latam_journal=="Latin American journal"]),
            w_n_latam = sum(Women[latam_journal!="Latin American journal"]),
            m_latam = sum(Men[latam_journal=="Latin American journal"]),
            m_n_latam = sum(Men[latam_journal!="Latin American journal"]))%>% 
  pivot_longer(!c("pub_year","level1"),names_to = "ind", values_to = "value")  %>% 
  mutate(Circuit = case_when(ind %in% c("w_latam","m_latam") ~ "In Latin America",
                             TRUE ~"Outside Latin America"))  %>% 
  mutate(gender = case_when(ind %in% c("w_latam","w_n_latam") ~ "Women",
                            TRUE ~"Men"))  %>% 
  mutate(level1 = factor(level1,levels = c("Natural Sciences", "Medical and Health Sciences","Engineering and Technology",  "Social Sciences" , "Agricultural Sciences"   , "Humanities" ) )) 

results_list$plot_1a_table <- plot_1a_table

#####figure 1b#####

plot_1b_table <-  latam_meta %>% 
  left_join(.,(latam_authors),
            by="Pub_ID") %>% 
  filter(country_code%in%latam_country_codes) %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  filter(gender %in% c("Men","Women")) %>% 
  select(Pub_ID,pub_year,author_id,gender) %>% 
  unique()%>% 
  group_by(pub_year) %>% 
  summarise(Women =n_distinct(author_id[gender=="Women"]),
            n = n_distinct(author_id)) %>% 
  ungroup() %>% 
  mutate("Women authors" = Women/n) %>% 
  select(-Women,-n) %>% 
  left_join(.,
            (latam_meta %>% 
               inner_join(.,(paper_level_tables$paper_gender_dist),
                          by="Pub_ID") %>% 
               left_join(.,journal_aux, by = c("Pub_ID" = "pub_id"))%>% 
               group_by(pub_year) %>% 
               summarise("Women authorship" =mean(Women)) )) %>% 
  pivot_longer(!pub_year, names_to = "ind", values_to = "value") 

results_list$plot_1b_table <- plot_1b_table

#####figure 1c#####

plot_1c_table <- latam_meta %>% 
  inner_join(.,(paper_level_tables$paper_gender_dist),
             by="Pub_ID") %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id"))%>% 
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
  pivot_longer(c("Total", "Women","Men"),names_to = "ind", values_to = "prop") 


results_list$plot_1c_table <- plot_1c_table

#####figure 1d#####

n_authors <- latam_meta %>% 
  left_join(.,(latam_authors),
            by="Pub_ID") %>% 
  filter(country_code%in%latam_country_codes) %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  filter(gender %in% c("Men","Women")) %>% 
  select(Pub_ID,pub_year,author_id,gender) %>% 
  unique()%>%
  group_by(pub_year) %>%
  summarise(Women =n_distinct(author_id[gender=="Women"]),
            Men =n_distinct(author_id[gender=="Men"])) %>%
  ungroup() |>
  pivot_longer(c("Women","Men"), names_to = "gender", values_to = "authors")

n_pubs <- latam_meta %>% 
  inner_join(.,(paper_level_tables$paper_gender_dist),
             by="Pub_ID") %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id"))%>%
  group_by(pub_year) %>%
  summarise(Women =sum(Women),
            Men =sum(Men))|>
  pivot_longer(c("Women","Men"), names_to = "gender", values_to = "pubs")

gender_productivity_gap <- n_authors |>
  left_join(n_pubs, by = c("pub_year","gender")) |>
  mutate(productivity = pubs/authors) |>
  select(-pubs,-authors) |>
  pivot_wider(id_cols = "pub_year", names_from = "gender", values_from = "productivity") |>
  mutate(gender_gap = (Men - Women)/Men)

plot_1d_table <-  latam_meta %>% 
  left_join(.,(latam_authors),
            by="Pub_ID") %>% 
  filter(country_code%in%latam_country_codes) %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  filter(gender %in% c("Men","Women")) %>% 
  select(Pub_ID,pub_year,author_id,gender) %>% 
  unique()%>% 
  group_by(pub_year) %>% 
  summarise(Women =n_distinct(author_id[gender=="Women"]),
            n = n_distinct(author_id)) %>% 
  ungroup() %>% 
  mutate("Women authors" = Women/n) %>% 
  select(-Women,-n) %>% 
  left_join(.,
            (latam_meta %>% 
               inner_join(.,(paper_level_tables$paper_gender_dist),
                          by="Pub_ID") %>% 
               left_join(.,journal_aux, by = c("Pub_ID" = "pub_id"))%>% 
               group_by(pub_year) %>% 
               summarise("Women authorship" =mean(Women)) )) %>% 
  mutate(gap = (`Women authors`- `Women authorship`)/`Women authors`) %>% 
  left_join(.,gender_productivity_gap, by = "pub_year") %>% 
  select(pub_year,"Women authors wrt women authorships" = "gap","Productivity gap" = "gender_gap")

results_list$plot_1d_table <- plot_1d_table


###figure 2####

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

plot_2a_table <- base_table %>% 
  filter(!is.na(level1)) |> 
  filter(!is.na(latam_journal)) %>% 
  group_by(latam_journal,level1) %>% 
  summarise(n = n_distinct(Pub_ID)) %>% 
  group_by(level1) %>% 
  mutate(prop = n/sum(n)) %>% 
  select(-n) %>% 
  left_join(.,order) %>% 
  mutate(latam_journal = case_when(latam_journal == "Latin American journal" ~"Published in Latin America",
                                   latam_journal == "Non Latin American journal" ~"Not published in Latin America"))

results_list$plot_2a_table <- plot_2a_table

#####figure 2b#####

plot_2b_table <- base_table %>% 
  left_join(.,latam_authors) %>% 
  filter(country_code%in%latam_country_codes) %>% 
  group_by(level1,gender) %>% 
  summarize(n = n_distinct(author_id)) %>% 
  filter(gender %in% c("Men","Women")) %>% 
  group_by(level1) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(!is.na(level1)) |>
  select(-n) %>%
  pivot_wider(.,id_cols = "level1", names_from = "gender", values_from = "prop") %>%
  mutate(ratio = Men/Women) %>%
  left_join(.,order)

results_list$plot_2b_table <- plot_2b_table

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

plot_2c_table <- real_1 %>% 
  left_join(.,exp_3) %>% 
  mutate(rel = real/exp -1) %>% 
  rename("Real" = "real",
         "Expected" = "exp",
         "Real/Expected" = "rel")

results_list$plot_2c_table <- plot_2c_table

###figure 3####

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
  left_join(.,interdis_correction) %>%
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


summary$level1[summary$level1 == "Medical And Health Sciences" ] <- "Medical and Health Sciences" 
summary$level1[summary$level1 == "Engineering And Technology" ] <- "Engineering and Technology" 

order$level1[order$level1 == "Medical And Health Sciences" ] <- "Medical and Health Sciences" 
order$level1[order$level1 == "Engineering And Technology" ] <- "Engineering and Technology" 

plot_3_table <- summary %>% 
  left_join(.,order) %>% 
  left_join(.,labeling_info)

results_list$plot_3_table <- plot_3_table

###figure 4####

order <- base_table %>% 
  filter(!is.na(level1))  %>% 
  group_by(level1) %>% 
  summarise(n = n_distinct(Pub_ID[latam_journal=="Latin American journal"])/n_distinct(Pub_ID[!is.na(latam_journal)])) %>% 
  arrange(-n) %>% 
  mutate(order = row_number()) %>% 
  select(-n)

#####figure 4a#####

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

plot_4a_table <- quants_topics %>% 
  select(topic,citations,quants_latam) |> 
  pivot_wider(id_cols = "topic", values_from = "citations", names_from = "quants_latam")

results_list$plot_4a_table <- plot_4a_table

#####figure 4b#####

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

plot_4b_table <-  gr_summary_quant_origin %>% 
  filter(!is.na(quants_latam)) %>% 
  filter(latam_author == "yes") %>% 
  select(topic,prop,quants_latam) |> 
  pivot_wider(id_cols = "topic", values_from = "prop", names_from = "quants_latam")

results_list$plot_4b_table <- plot_4b_table

#####figure 4c#####

bootstrap_coefficients <-readRDS("results/bootstrap_confidence.RDS")
bootstrap_coefficients$level1[bootstrap_coefficients$level1 == "Medical And Health Sciences" ] <- "Medical and Health Sciences" 
bootstrap_coefficients$level1[bootstrap_coefficients$level1 == "Engineering And Technology" ] <- "Engineering and Technology" 

plot_4c_table <- base_table %>% 
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
                                  latam_journal == "Not published in Latin America" & Gender == "Women" ~ "Women - Not published in Latin America"))   
  

results_list$plot_4c_table <- plot_4c_table

###save####
saveRDS(results_list,"app/www/results_list.RDS")
