unique(ffig1a_table$level1)

ffig1a_table%>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(pub_year,level1) %>% 
  summarise(w_latam = sum(Women[latam_journal=="Latin American journal"]),
            w_n_latam = sum(Women[latam_journal!="Latin American journal"]),
            m_latam = sum(Men[latam_journal=="Latin American journal"]),
            m_n_latam = sum(Men[latam_journal!="Latin American journal"]))%>% 
  # summarise("In Latin America" = n_distinct(Pub_ID[latam_journal=="Latin American journal"]),
  #           "Outside Latin America"= n_distinct(Pub_ID[latam_journal!="Latin American journal"])
  pivot_longer(!c("pub_year","level1"),names_to = "ind", values_to = "value")  %>% 
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
  theme(axis.text.x = element_text(size =test_size_fig1))+
  theme(axis.text.y = element_text(size =test_size_fig1))+
  theme(legend.text = element_text(size =test_size_fig1))+
  scale_color_brewer(palette = "Paired",direction = 1)+
  labs(y = "Number of publications",
       x = "Publication year", fill = "", color = "", linetype = "",
       title= "Number of publications by gender, circuit and discipline \nfor Latin-American researchers (fractional counts)")+
  facet_wrap(~level1,scales = "free")

ggsave('results/figures/reviewers_comment.png', width = 10, height = 10,bg = "white")

###supplementary######

####Fig a####

ffig1a_table <- latam_meta %>% 
  inner_join(.,(paper_level_tables$paper_gender_dist),
             by="Pub_ID") %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id"))

ffig1a_table%>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(pub_year,level1) %>% 
  summarise(w_latam = sum(Women[latam_journal=="Latin American journal"]),
            w_n_latam = sum(Women[latam_journal!="Latin American journal"]),
            m_latam = sum(Men[latam_journal=="Latin American journal"]),
            m_n_latam = sum(Men[latam_journal!="Latin American journal"]))%>% 
  # summarise("In Latin America" = n_distinct(Pub_ID[latam_journal=="Latin American journal"]),
  #           "Outside Latin America"= n_distinct(Pub_ID[latam_journal!="Latin American journal"])
  pivot_longer(!c("pub_year","level1"),names_to = "ind", values_to = "value")  %>% 
  mutate(Circuit = case_when(ind %in% c("w_latam","m_latam") ~ "In Latin America",
                             TRUE ~"Outside Latin America"))  %>% 
  mutate(gender = case_when(ind %in% c("w_latam","w_n_latam") ~ "Women",
                            TRUE ~"Men"))  %>% 
  mutate(level1 = factor(level1,levels = c("Natural Sciences", "Medical and Health Sciences","Engineering and Technology",  "Social Sciences" , "Agricultural Sciences"   , "Humanities" ) )) %>% 
  ggplot(.,aes(y=value,x=as.numeric(pub_year),group = ind,color = gender, linetype = Circuit ))+
  geom_line()+
  #geom_vline(aes(xintercept = 2003), size = .2, linetype = "longdash",alpha=.5)+
  #geom_vline(aes(xintercept = 2012), size = .2, linetype = "longdash",alpha=.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size =test_size_fig1))+
  theme(axis.text.x = element_text(size =test_size_fig1))+
  theme(axis.text.y = element_text(size =test_size_fig1))+
  theme(legend.text = element_text(size =test_size_fig1))+
  scale_color_brewer(palette = "Paired",direction = 1)+
  labs(y = "Number of publications",
       x = "Publication year", fill = "", color = "", linetype = "",
       title= "")+
  facet_wrap(~level1,scales = "free")

ggsave('results/figures/supplementary/fig_1A_disag.tiff', width = 10, height = 8, dpi = 350,bg = "white", device='tiff')


####Fig b####

ffig1b_table <- latam_meta %>% 
  left_join(.,(latam_authors),
            by="Pub_ID") %>% 
  filter(country_code%in%latam_country_codes) %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  filter(gender %in% c("Men","Women")) %>% 
  select(Pub_ID,pub_year,level1,author_id,gender) %>% 
  unique()

ffig1b_table%>% 
  group_by(pub_year,level1) %>% 
  summarise(Women =n_distinct(author_id[gender=="Women"]),
            n = n_distinct(author_id)) %>% 
  ungroup() %>% 
  mutate("Women authors" = Women/n) %>% 
  select(-Women,-n) %>% 
  left_join(.,
            (ffig1a_table%>% 
               group_by(pub_year,level1) %>% 
               summarise("Women authorship" =mean(Women)) )) %>% 
  pivot_longer(!c("pub_year","level1"), names_to = "ind", values_to = "value") %>% 
  
  ggplot(.,aes(y=value,x=as.numeric(pub_year),group = ind,color = ind, linetype = ind))+
  geom_line()+
  #geom_point()+
  #geom_vline(aes(xintercept = 2003), size = .2, linetype = "longdash",alpha=.5)+
  #geom_vline(aes(xintercept = 2012), size = .2, linetype = "longdash",alpha=.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size = test_size_fig1))+
  theme(axis.text.x = element_text(size =test_size_fig1))+
  theme(axis.text.y = element_text(size =test_size_fig1))+
  theme(legend.text = element_text(size =test_size_fig1))+
  scale_color_viridis(discrete = TRUE,option = "F", begin = .2, end =.5)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(x = "Publication year", 
       y = "Women authors / \n Women authorship",
       color = "",  linetype = "",
       title = ""
  )+
  #geom_hline(yintercept = .5, size=.1)+
  guides(fill = guide_legend(reverse=TRUE))+
  facet_wrap(~level1)

ggsave('results/figures/supplementary/fig_1B_disag.tiff', width = 10, height = 10, dpi = 350,bg = "white", device='tiff')

####Fig c####


ffig1a_table%>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(pub_year,level1) %>% 
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
  theme(axis.text.x = element_text(size =test_size_fig1))+
  theme(axis.text.y = element_text(size =test_size_fig1))+
  theme(legend.text = element_text(size =test_size_fig1))+
  scale_color_manual(values = c("#A6CEE3", "black", "#1F78B4"))+
  scale_linetype_manual(values=c("dashed", "solid", "dashed")) +  
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(y = "Published in Latin American \n journals or conferences",
       x = "Publication year", color = "",linetype ="",
       title= "")+
  facet_wrap(~level1)

ggsave('results/figures/supplementary/fig_1C_disag.tiff', width = 10, height = 10, dpi = 350,bg = "white", device='tiff')

###b,c,d figure#####

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
  theme(legend.position = c(0.2, 0.95))+
  theme(text = element_text(size = test_size_fig1))+
  theme(axis.text.x = element_text(size =test_size_fig1))+
  theme(axis.text.y = element_text(size =test_size_fig1))+
  theme(legend.text = element_text(size =test_size_fig1))+
  scale_color_viridis(discrete = TRUE,option = "F", begin = .2, end =.5)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(x = "Publication year", 
       y = "Women authors / \n Women authorship",
       color = "",  linetype = "",
       title = "A"
  )+
  guides(fill = guide_legend(reverse=TRUE))

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
  mutate(ind = factor(ind,levels = c("Women","Total", "Men")))|>
  ggplot(aes(y=prop,x=as.numeric(pub_year),color = ind,group=ind ,linetype = ind ))+
  geom_line()+
  # geom_vline(aes(xintercept = 2003), size = .2, linetype = "longdash",alpha=.5)+
  #  geom_vline(aes(xintercept = 2012), size = .2, linetype = "longdash",alpha=.5)+
  theme_minimal()+
  theme(legend.position = c(0.95, 0.90))+
  theme(text = element_text(size =test_size_fig1))+
  theme(axis.text.x = element_text(size =test_size_fig1))+
  theme(axis.text.y = element_text(size =test_size_fig1))+
  theme(legend.text = element_text(size =test_size_fig1))+
  scale_color_manual(values = c("#A6CEE3", "black", "#1F78B4"))+
  scale_linetype_manual(values=c("dashed", "solid", "dashed")) +  
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(y = "Published in Latin American \n journals or conferences",
       x = "Publication year", color = "",linetype ="",
       title= "C")

plot_d <-ffig1b_table%>% 
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
  left_join(.,gender_productivity_gap, by = "pub_year") %>% 
  select(pub_year,"Women authors wrt women authorships" = "gap","Productivity gap" = "gender_gap") %>% 
  #pivot_longer(!pub_year, names_to = "ind", values_to = "value") %>% 
  ggplot(.,aes(x=as.numeric(pub_year)))+
  geom_point(aes(y = `Women authors wrt women authorships`),size=.2,color = "#6A3D9A")+
  geom_point(aes(y = `Productivity gap`*coeff),size=.2,color = "#33A02C")+
  geom_smooth(aes(y = `Women authors wrt women authorships`), size=.5,color = "#6A3D9A",fill = "lightgray")+
  geom_smooth(aes(y = `Productivity gap`*coeff), size=.5,color = "#33A02C",fill = "lightgray")+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),
                     name = "Gap between women authors \n and women authorship",
                     sec.axis = sec_axis(~./coeff, name="Gender productivity gap",
                                         labels = function(x) paste0(x*100,"%")))+
  
  annotate( "text",x=2005, y=.145 , label="Women authors \n wrt women authorships", color="#6A3D9A",size=3) +
  annotate( "text",x=2015, y=.115, label="Productivity gap", color="#33A02C",size=3) +
  theme_minimal()+
  theme(text = element_text(size = test_size_fig1))+
  theme(axis.text.x = element_text(size =test_size_fig1))+
  theme(axis.text.y = element_text(size =test_size_fig1))+
  theme(legend.text = element_text(size =test_size_fig1))+
  #scale_color_viridis(discrete = TRUE,option = "F", begin = .2, end =.5)+
  labs(x = "Publication year", 
       title = "B"
  )

plot_b + (plot_d / plot_c)  + plot_layout(widths = c(1/2,1/2))

ggsave('results/figures/supplementary/latam_context_bcd.tiff', width = 10, height = 8, dpi = 350,bg = "white", device='tiff')


###experiment####

ffig1a_table%>% 
  filter(!is.na(latam_journal)) %>% 
  group_by(pub_year) %>% 
  summarise(w_n_latam = sum(Women[latam_journal!="Latin American journal"]),
            m_n_latam = sum(Men[latam_journal!="Latin American journal"]),
            m_latam = sum(Men[latam_journal=="Latin American journal"]),
            w_latam = sum(Women[latam_journal=="Latin American journal"])) |>
  mutate(w_n_growth = (w_n_latam - lag(w_n_latam))/w_n_latam,
         m_n_growth = (m_n_latam - lag(m_n_latam))/m_n_latam,
         w_growth = (w_latam - lag(w_latam))/w_latam,
         m_growth = (m_latam - lag(m_latam))/m_latam) %>% 
  select(-w_n_latam,-m_n_latam,-m_latam,-w_latam) %>% 
  pivot_longer(!pub_year,names_to = "ind", values_to = "value") %>% 
  mutate(Circuit = case_when(ind %in% c("w_growth","m_growth") ~ "In Latin America",
                             TRUE ~"Outside Latin America"))  %>% 
  mutate(gender = case_when(ind %in% c("w_growth","w_n_growth") ~ "Women",
                            TRUE ~"Men"))%>% 
  ggplot(.,aes(y=value,x=as.numeric(pub_year),group = ind,color = gender, linetype = Circuit ))+
  geom_line()+
  #geom_vline(aes(xintercept = 2003), size = .2, linetype = "longdash",alpha=.5)+
  #geom_vline(aes(xintercept = 2012), size = .2, linetype = "longdash",alpha=.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size =test_size_fig1))+
  theme(axis.text.x = element_text(size =test_size_fig1))+
  theme(axis.text.y = element_text(size =test_size_fig1))+
  theme(legend.text = element_text(size =test_size_fig1))+
  scale_color_brewer(palette = "Paired",direction = 1)
