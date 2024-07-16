library(boot)

#note: confidence interval is by default 95%: boot.ci( conf = 0.95...
#we perform 1000 bootstrap resamples

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

aux_journal_country <- readRDS("results/aux_journal_country_1990.RDS")%>% 
  select(-journal_country) %>% 
  unique()

aux_conference_country <- readRDS("results/aux_conference_country_1990.RDS") %>% 
  select(-proc_country) %>% 
  unique() %>% 
  rename("latam_journal" = "latam_conf") %>% 
  janitor::clean_names()

journal_aux <- bind_rows(aux_journal_country,aux_conference_country) %>% unique()

document_topics <- read_csv("data/topic_model/bertopic_output_20240302_195213/document_topics.csv")


base_table <- document_topics %>% 
  left_join(.,journal_aux, by = c("Pub_ID" = "pub_id")) %>% 
  left_join(.,(latam_meta %>% 
                 distinct(Pub_ID,level1,normalized_citations) )) %>% 
  left_join(paper_level_tables$paper_gender_dist) %>% 
  filter(Topic != -1) %>% 
  rename("topic" = "Topic")


boot_base <-  base_table %>% 
  filter(!is.na(level1)) %>% 
  filter(!is.na(latam_journal)) %>% 
  filter(!is.na(Women)) 

set.seed(1997)

# Function to calculate weighted mean
weighted_mean_women <- function(data, indices) {
  d <- data[indices, ]
  return(weighted.mean(d$normalized_citations, d$Women,na.rm = TRUE))
}

weighted_mean_men <- function(data, indices) {
  d <- data[indices, ]
  return(weighted.mean(d$normalized_citations, d$Men,na.rm = TRUE))
}


# Function to calculate bootstrap confidence intervals
bootstrap_ci <- function(group_data, R = 10) {
  boot_results <- boot(data = group_data, statistic = weighted_mean_women, R = R)
  ci <- boot.ci(boot_results, type = "perc")
  return(data.frame(weighted_mean = mean(boot_results$t), lower_ci = ci$percent[4], upper_ci = ci$percent[5]))
}

# Group by the 'group' variable and calculate bootstrap confidence intervals
results_women <- boot_base %>%
  group_by(latam_journal,level1) %>%
  group_modify(~ bootstrap_ci(.x, R = 1000))


# Function to calculate bootstrap confidence intervals
bootstrap_ci_m <- function(group_data, R = 10) {
  boot_results <- boot(data = group_data, statistic = weighted_mean_men, R = R)
  ci <- boot.ci(boot_results, type = "perc")
  return(data.frame(weighted_mean = mean(boot_results$t), lower_ci = ci$percent[4], upper_ci = ci$percent[5]))
}

# Group by the 'group' variable and calculate bootstrap confidence intervals
results_men <- boot_base %>%
  group_by(latam_journal,level1) %>%
  group_modify(~ bootstrap_ci_m(.x, R = 1000))


#format
results_women <-  results_women |> 
  mutate(Gender = "Women")

results_men <-  results_men |> 
  mutate(Gender = "Men")

bootstrap_coefficients <- bind_rows(results_women,results_men) 

saveRDS(bootstrap_coefficients,"results/bootstrap_confidence.RDS")
