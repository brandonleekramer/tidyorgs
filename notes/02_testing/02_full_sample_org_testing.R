

rm(list=ls())
library("tidyverse")
library("RPostgreSQL")
load_all()

conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
github_users <- dbGetQuery(conn, "SELECT login, company, email 
                           FROM gh.ctrs_clean_0821")
dbDisconnect(conn)

# classification of academic organizations 
classified_orgs <- github_users %>%
  detect_orgs(login, company, organization, academic, email)

academic_users <- classified_orgs %>% 
  filter(academic == 1)
  
academic_counts <- academic_users %>% 
  separate_rows(organization, sep = "\\|") %>% 
  group_by(organization) %>%   
  count() %>% 
  arrange(-n)

# 20002 -> 15000 -> 14362 -> 12526
misc_academic <- academic_users %>% 
  filter(organization == "misc. academic")

chk_misc <- misc_academic %>% 
  mutate(company = tolower(company)) %>% 
  group_by(company) %>% 
  count() %>% 
  arrange(-n)
#write_csv(misc_academic, "data-raw/misc_academic.csv")

academic_raw <- readr::read_rds(file = "R/academic_instiutions.rds") %>% 
  select(new_string, country) %>% 
  rename(organization = new_string)

academic_plus_email <- academic_users %>% 
  left_join(academic_raw, by = "organization")

chk <- academic_plus_email %>% 
  janitor::get_dupes(login)

chk <- chk %>% distinct(organization, country)

counts_by_country <- academic_plus_email %>% 
  group_by(country) %>% 
  count() %>% 
  arrange(-n)

counts_by_country <- counts_by_country %>% 
  mutate(total = sum(counts_by_country$n), 
         fraction = round(n / total, 2))

academic_users <- academic_users %>% 
  left_join(academic_raw, by = "organization")
 







