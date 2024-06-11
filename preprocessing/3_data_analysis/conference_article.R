library(tidyverse)

paper_level_tables <- readRDS('results/paper_level_tables_1990.RDS')


aux_journal_country <- readRDS("results/aux_journal_country_1990.RDS")%>% 
  select(pub_id) %>% 
  unique() %>% 
  mutate(pub_type = "Article")

aux_conference_country <- readRDS("results/aux_conference_country_1990.RDS") %>% 
  select(Pub_ID) %>% 
  unique() %>% 
  janitor::clean_names() %>% 
  mutate(pub_type = "Conference")

conf_art <- bind_rows(aux_journal_country,aux_conference_country)

conf_art <- conf_art %>% 
  left_join(paper_level_tables$paper_gender_dist, by = c("pub_id" = "Pub_ID"))

latam_meta <- read_delim("data/latam/latam_meta_1990.csv", 
                         delim = ";", escape_double = FALSE, 
                         col_names = c('Pub_ID', 'pub_year','source_title', 'source_type',
                                       'level1', 'level2', 'n_cits'), 
                         trim_ws = TRUE
                         
)  %>% 
  select('Pub_ID','level1', 'pub_year') %>% 
  distinct(Pub_ID, .keep_all = TRUE) %>% 
  filter(!is.na(level1)) 



summary_conf_art <- conf_art %>% 
  filter(!is.na(Women)) %>% 
  left_join(latam_meta, by = c("pub_id" = "Pub_ID")) %>% 
  filter(!is.na(level1)) %>% 
  group_by(pub_type,level1) %>% 
  summarise(w=sum(Women),
            m = sum(Men)) %>% 
  pivot_longer(c("w","m"), names_to = "gender", values_to = "n") %>% 
  group_by(pub_type,level1) %>% 
  mutate(prop = n/sum(n))

summary_conf_art %>% 
  ggplot(aes(x= prop,y=pub_type, fill = gender))+
  geom_col()+
  facet_wrap(~level1)+
  theme_minimal()+
  labs(x = "authorship")+
  scale_fill_brewer(palette = "Paired")

summary_conf_art %>% 
  ggplot(aes(x= n,y=pub_type, fill = gender))+
  geom_col(position = "dodge")+
  facet_wrap(~level1)+
  theme_minimal()+
  labs(x = "authorship")+
  scale_fill_brewer(palette = "Paired")

summary_conf_art_tot <- conf_art %>% 
  filter(!is.na(Women)) %>% 
  left_join(latam_meta, by = c("pub_id" = "Pub_ID")) %>% 
  filter(!is.na(level1)) %>% 
  group_by(pub_type) %>% 
  summarise(w=sum(Women),
            m = sum(Men)) %>% 
  pivot_longer(c("w","m"), names_to = "gender", values_to = "n") %>% 
  group_by(pub_type) %>% 
  mutate(prop = n/sum(n))

summary_conf_art_evol <- conf_art %>% 
  filter(!is.na(Women)) %>% 
  left_join(latam_meta, by = c("pub_id" = "Pub_ID")) %>% 
  filter(!is.na(level1)) %>% 
  mutate(period = factor(case_when(pub_year < 2010 ~ "pre-2010",
                            TRUE ~"post-2010"),
                         levels = c("pre-2010","post-2010"))) %>% 
  group_by(pub_type,level1,period) %>% 
  summarise(w=sum(Women),
            m = sum(Men)) %>% 
  pivot_longer(c("w","m"), names_to = "gender", values_to = "n") %>% 
  group_by(pub_type,level1,period) %>% 
  mutate(prop = n/sum(n))

summary_conf_art_evol%>% 
  ggplot(aes(x= period,y=prop, fill = gender))+
  geom_col()+
  facet_grid(pub_type~level1)+
  theme_minimal()+
  labs(x = "authorship")+
  scale_fill_brewer(palette = "Paired")
  


