 
##### location testing 

rm(list=ls())
library("tidyverse")
library("RPostgreSQL")
load_all()

# reconnecting to the database 
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
github_users <- dbGetQuery(conn, "SELECT login, location, email, company
                           FROM gh.ctrs_clean_0821 LIMIT 1000")
academic_classified <- dbGetQuery(conn, "SELECT login, organization, country, academic
                           FROM gh.sna_ctr_academic_0821")
dbDisconnect(conn)

# testing sub-algos first - they work 
text_to_countries_df <- github_users %>%
  text_to_countries(login, location, country_name)
email_to_countries_df <- github_users %>%
  email_to_countries(login, email, country_name)

load_all()
# joining classified academic country information first 
detected_countries_df <- github_users %>% 
#russian_classified <- github_users %>%
  #left_join(academic_classified, by = "login") %>% 
  #unite("location", c("location", "country"), sep = " ") %>% 
  #mutate(location = str_replace(location, " NA", "")) %>%
  #filter(grepl("russia", location)) %>% 
  detect_countries(login, location, country_name, email) 


#dictionary <- tidyorgs::countries_data %>% 
dictionary <- read_csv("data-raw/tidyorgs - countries.csv")%>% 
  separate_rows(catch_terms, sep = "\\|") %>%
  janitor::get_dupes(catch_terms) %>% 
  select(catch_terms, country_name, dupe_count)



only_detected <- detected_countries_df %>% 
  drop_na(country_name)

not_detected_with_valid_data <- detected_countries_df %>% 
  ungroup() %>% 
  filter(is.na(country_name)) %>% 
  select(-email) %>% 
  mutate(location = na_if(location, "NA")) %>% 
  filter(!is.na(location) | !is.na(company))

bigrams <- not_detected %>% 
  tidytext::unnest_tokens(bigram, location, token = "ngrams", n = 2) %>% 
  group_by(bigram) %>% 
  count() %>% 
  arrange(-n)
         
trigrams <- not_detected %>% 
  tidytext::unnest_tokens(bigram, location, token = "ngrams", n = 3) %>% 
  group_by(bigram) %>% 
  count() %>% 
  arrange(-n)
  
detected_counts <- only_detected %>% 
  separate_rows(country_name, sep = "\\|") %>% 
  group_by(country_name) %>% 
  count() %>% 
  arrange(-n)

with_valid_data <- github_users %>%
  filter(!is.na(location) | !is.na(company) | !is.na(email))

nonvalid_data <- github_users %>%
  filter(is.na(location) & is.na(company) & is.na(email)) 

table_to_database <- detected_countries_df %>% 
  left_join(academic_classified, by = "login") %>% 
  rename(country_raw = country) %>% 
  select(login, organization, country_name, academic, email, company, location, country_raw) %>% 
  mutate(academic = replace_na(academic, 0)) 

# write that table to the database 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
dbWriteTable(conn, c("gh", "ctrs_classified_0821"), 
             table_to_database, row.names = FALSE)
dbDisconnect(conn)













