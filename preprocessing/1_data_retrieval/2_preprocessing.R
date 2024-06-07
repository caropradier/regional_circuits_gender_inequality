library(tidyverse)
library(readxl)
library(countrycode)
library(countrycode)
library(datasets)
library(maps)

latam_authors <- read_delim("data/latam/latam_authors_1990.csv",
                            delim = ";", escape_double = FALSE,
                            #col_names = TRUE,
                            col_names = c('Pub_ID','author_seq',
                                          'country_code','city','author_id',
                                          'first_name','last_name','gender'),
                            trim_ws = TRUE
)%>% 
  distinct(Pub_ID, author_id,country_code,gender,.keep_all = TRUE) 

latam_meta <- read_delim("data/latam/latam_meta_1990.csv", 
                         delim = ";", escape_double = FALSE, 
                         #col_names = TRUE, 
                         col_names = c('Pub_ID', 'pub_year','source_title', 'source_type',
                                       'level1', 'level2', 'n_cits'),
                         trim_ws = TRUE
                         
)

names(latam_meta)
names(latam_authors)


gender_assign <- latam_authors %>% 
  left_join(.,latam_meta) %>% 
  group_by(pub_year, gender) %>% 
  summarise(n = n_distinct(author_id)) %>% 
  group_by(pub_year) %>% 
  mutate(prop = n/sum(n))

# gender_assign %>% 
#   ggplot(.,aes(x = pub_year,y = prop, fill = gender))+
#   geom_col(position = "stack")

#lots of nulls before 2008

# export_nulls <- latam_authors %>% 
#   filter(gender == "NULL") %>% 
#   select(author_id)

#saveRDS(export_nulls,"null_authors.RDS")

full_inference <- read_delim("data/latam/dimensions_inference_1990.csv", 
                             delim = ",", escape_double = FALSE, trim_ws = TRUE)

latam_authors <- latam_authors %>% 
  select(-gender) %>% 
  left_join(.,full_inference)

# export_nulls2 <- latam_authors %>% 
#      filter(is.na(gender)) %>% 
#     select(author_id) %>% 
#     unique()

#saveRDS(export_nulls2,"null_authors.RDS")

gender_assign <- latam_authors %>% 
  left_join(.,latam_meta) %>% 
  group_by(pub_year, gender) %>% 
  summarise(n = n_distinct(author_id)) %>% 
  group_by(pub_year) %>% 
  mutate(prop = n/sum(n))


# gender_assign %>% 
#   mutate(gender = factor(case_when(gender == "INI"~"Initiales",
#                                    gender == "UNI" | gender == "UNK"~"Inconnu",
#                                    gender == "Men"~"Homme",
#                                    gender == "Women"~"Femme"),
#                          levels = c("Initiales","Inconnu","Femme","Homme"))) %>% 
#   group_by(gender,pub_year) %>% 
#   mutate(n = sum(n),
#          prop = sum(prop)) %>% 
#   unique() %>% 
#   ggplot(.,aes(x = pub_year,y = prop, fill = gender))+
#   geom_col(position = "stack")+
#   theme_minimal()+
#   scale_fill_viridis(discrete = TRUE)+
#   scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
#   labs(fill = "Genre inféré",x = "Année de publication",y="")

# gender_assign %>% 
#   filter(pub_year%in% c(1993:2002)) %>% 
#   group_by(gender) %>% 
#   summarise(n = sum(n)) %>% 
#   mutate(prop = n/sum(n))

saveRDS(latam_authors,"data/latam/latam_authors_1990.RDS")


##

latam_meta <- read_delim("data/latam/latam_meta_1990.csv", 
                         delim = ";", escape_double = FALSE, 
                         #col_names = TRUE, 
                         col_names = c('Pub_ID', 'pub_year','source_title', 'source_type',
                                      'level1', 'level2', 'n_cits'),
                         trim_ws = TRUE
                         
)

latam_ids <- read_delim("data/latam/latam_ids_1990.csv", 
                         delim = ";", escape_double = FALSE, 
                         col_names = TRUE, 
                         trim_ws = TRUE
                         
)

latam_country_codes <- c('AR','BO','BR','CL','CO','MX','PY','PE','UY','VE')

####paper_level_tables#####

paper_level_tables <- list()

paper_gender_dist <- latam_authors %>%
  filter(country_code%in%latam_country_codes) %>% 
  filter(gender %in% c('Men','Women')) %>% 
  group_by(Pub_ID, gender) %>% 
  summarise(n=n_distinct(author_id)) %>% 
  group_by(Pub_ID) %>% 
  mutate(p = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = gender, values_from = p, values_fill = 0)

paper_level_tables$paper_gender_dist <- paper_gender_dist

#only latinamerican authors

paper_gender_dist <- latam_authors %>%
  filter(country_code %in%latam_country_codes)

length(unique(paper_gender_dist$author_id))
#2736608

paper_gender_dist |> 
  group_by(gender) |> 
  summarise(n = n_distinct(author_id)) |> 
  mutate(prop = n/sum(n))
#gender can be inferred for 72.5%


#authorship of only latin american authors
paper_latam_dist <- latam_authors %>%
  filter(!is.na(country_code)) |> 
  mutate(latam_author = ifelse(country_code %in% latam_country_codes, "Latin American author", "Non Latin American author")) |> 
   group_by(Pub_ID, latam_author) %>% 
   summarise(n=n_distinct(author_id)) %>% 
   group_by(Pub_ID) %>% 
   mutate(p = n/sum(n)) %>% 
   select(-n) %>% 
   pivot_wider(names_from = latam_author, values_from = p, values_fill = 0)

sum(paper_latam_dist$`Latin American author`)/(sum(paper_latam_dist$`Latin American author`) + sum(paper_latam_dist$`Non Latin American author`))
#0.8764687

aux_gender_props <-  latam_authors %>% 
  filter(gender %in% c("Men","Women")) %>% 
  group_by(Pub_ID,gender) %>% 
  summarise(n = n_distinct(author_id)) %>% 
  group_by(Pub_ID) %>% 
  mutate(n = n/sum(n))  %>% 
  pivot_wider(id_cols = "Pub_ID", names_from = "gender", values_from = "n",values_fill = 0) %>% 
  mutate(comp = factor(case_when(Men > Women & Men != 1  ~ "Women minority",
                                 Women >= Men & Women != 1 ~ "Women majority",
                                 Women == 1 ~ "Only women",
                                 Men == 1 ~ "Only men"),
                       levels=c("Only men","Women minority","Women majority","Only women"))) %>% 
  select(Pub_ID,"gender_comp"="comp")

paper_level_tables$aux_gender_props <- aux_gender_props

aux_number_authors <- latam_authors %>% 
  group_by(Pub_ID) %>% 
  summarise(n_collaborators = n_distinct(author_id)) 

paper_level_tables$aux_number_authors <- aux_number_authors

aux_first_author <- latam_authors %>% 
  group_by(Pub_ID) %>% 
  #keep first author
  filter(author_seq == min(author_seq)) %>% 
  #if more than one author is identified as first author but it's the same person, that's ok
  distinct(Pub_ID,author_id, country_code,gender,.keep_all = TRUE) %>% 
  #otherwise, I remove them
  group_by(Pub_ID) %>%
  filter(n() == 1) %>% 
  select(Pub_ID,"first_author_id" = "author_id","first_author_gender" = "gender","first_author_country_code" = "country_code")

paper_level_tables$aux_first_author <- aux_first_author

saveRDS(paper_level_tables,"results/paper_level_tables_1990.RDS")

###authors_list#####

authors_list <- list()

paper_gender_dist <- latam_authors %>%
  filter(country_code%in%latam_country_codes) %>% 
  filter(gender %in% c('Men','Women')) %>% 
  group_by(Pub_ID, gender) %>% 
  summarise(n=n_distinct(author_id)) %>% 
  group_by(Pub_ID) %>% 
  mutate(p = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = gender, values_from = p, values_fill = 0)

general <- paper_gender_dist %>% 
  ungroup() %>% 
  summarise(Men = mean(Men),
            Women = mean(Women)) %>% 
  mutate(country_code = "World")

paper_gender_country_dist <- latam_authors %>%
  filter(country_code%in%latam_country_codes) %>% 
  filter(gender %in% c('Men','Women')) %>% 
  group_by(country_code,Pub_ID, gender) %>% 
  summarise(n=n_distinct(author_id)) %>% 
  group_by(Pub_ID) %>% 
  mutate(p = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = gender, values_from = p, values_fill = 0)

by_country <- paper_gender_country_dist %>% 
  group_by(country_code) %>% 
  summarise(Men = sum(Men),
            Women = sum(Women)) %>% 
  mutate(tot = Men+Women,
         Men = Men/tot,
         Women = Women/tot) %>% 
  select(-tot)

frac_gender_country <- bind_rows(by_country,general)

authors_list$authors_frac_country <- frac_gender_country

saveRDS(authors_list,"results/authors_analysis_1990.RDS")

####journals####

ulrichs_df <- read_excel("data/latam/ulrichs full db.xlsx")

ulrichs_df <- ulrichs_df %>% 
  select(ISSN,Country) %>% 
  distinct()

df <- latam_ids %>% 
  inner_join(ulrichs_df, by=join_by(issn_print==ISSN))

df <- df %>% 
  select(-doi, -issn_print) %>% 
  unique()

df <- df %>% 
  rename("journal_country" = "Country")

df <- df %>% 
mutate(journal_country = countrycode(journal_country, origin = 'country.name', destination = 'iso2c')) %>% 
  mutate(latam_journal = ifelse(journal_country %in% latam_country_codes, "Latin American journal", "Non Latin American journal")) %>% unique()

saveRDS(df, "results/aux_journal_country_1990.RDS")

####conferences####

latam_conference_loc_1990 <- read_delim("data/latam/latam_conference_loc_1990.csv", 
                                        delim = ";", escape_double = FALSE, col_names = c('Pub_ID', 'conference_id','conference_location_id','conference_location'), 
                                        trim_ws = TRUE)

locations <- latam_conference_loc_1990 %>% 
  select(conference_location_id,conference_location) %>% unique()

country_names <- countrycode::countryname_dict

regexs <- countrycode::codelist$country.name.en.regex

us_states <- c(state.abb,state.name)

cities_table <- maps::world.cities%>% select(name, country.etc,pop) %>% 
  filter(!grepl("'",name))

cities <- cities_table$name

country_vector <- c(unique(country_names$country.name.en),
                    "USA", "UK", "England","Korea", 
                    "Virtual", "Online",
                    regexs)

#countries

locations_clean <- locations %>% 
  mutate(conference_location = str_replace_all(conference_location, c("á" = "a", 
                                                                      "é" = "e",
                                                                      "í" = "i",
                                                                      "ó" = "o",
                                                                      "ú" = "u"))) %>% 
  mutate(country = str_extract(conference_location,paste(country_vector, collapse="|"))) 

#us states

locations_clean2 <-  locations_clean %>% 
  filter(is.na(country)) %>% 
  mutate(state = str_extract(conference_location,paste(us_states, collapse="|"))) %>%  
  mutate(country = case_when(!is.na(state)~ "United States")) %>% 
  select(-state)

#cities

locations_clean3 <-  locations_clean2 %>% 
  filter(is.na(country)) %>% 
  select(-country) %>% 
  mutate(city = str_extract(conference_location,paste(cities, collapse="|"))) %>%  
  left_join(.,cities_table, by = c("city" = "name")) %>% 
  rename("country" = "country.etc") %>% 
  group_by(conference_location_id,conference_location) %>% 
  slice_max(order_by = pop, n = 1) %>% 
  select(-pop, -city)

#unidentified

locations_clean4 <- locations_clean3 %>% 
  filter(is.na(country)) %>% 
  mutate(country = case_when(conference_location == "Buzios"| grepl("Janeiro",conference_location) ~ paste0("Brazil"),
                             TRUE ~ "Not Latam (Ins Inf)"))

cleaned_version <- bind_rows(locations_clean4 %>% filter(!is.na(country)),
                             locations_clean3 %>% filter(!is.na(country)),
                             locations_clean2 %>% filter(!is.na(country)),
                             locations_clean %>% filter(!is.na(country)))

country_to_code <- cleaned_version %>% 
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso2c'))

#saveRDS(country_to_code, "data/latam/conference_proceedings_countries.RDS")

#country_to_code <- readRDS( "data/latam/conference_proceedings_countries.RDS")

latam_conference_loc_1990 <- latam_conference_loc_1990 %>% 
  left_join(.,(country_to_code %>% select(-conference_location))
  )
latam_country_codes <- c('AR','BO','BR','CL','CO','MX','PY','PE','UY','VE')

latam_conference_loc_1990 <- latam_conference_loc_1990 %>% 
  select(Pub_ID, "proc_country"="country_code") %>% 
  mutate(latam_conf = ifelse(proc_country %in% latam_country_codes, "Latin American journal", "Non Latin American journal")) 

saveRDS(latam_conference_loc_1990,"results/aux_conference_country_1990.RDS")


