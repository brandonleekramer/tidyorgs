
rm(list=ls())
library("tidyverse")
#library("tidyorgs")
library("devtools")

setwd("~/Documents/git/oss-2020/data")
uva_scraped_data <- readRDS("github_sectored_101321.rds") 
uva_scraped_data <- uva_scraped_data %>% 
  select(login, company, location, email)

load_all()
classified_academic <- uva_scraped_data %>%
  detect_academic(login, company, organization, email) %>% 
  filter(academic == 1)
classified_businesses <- uva_scraped_data %>%
  detect_business(login, company, organization, email) %>% 
  filter(business == 1)
classified_government <- uva_scraped_data %>%
  detect_government(login, company, organization, email) %>% 
  filter(government == 1)
classified_nonprofit <- uva_scraped_data %>%
  detect_nonprofit(login, company, organization, email) %>% 
  filter(nonprofit == 1)
classified_orgs <- bind_rows(
  classified_academic, classified_businesses,
  classified_government, classified_nonprofit) %>% 
  mutate(academic = replace_na(academic, 0)) %>% 
  mutate(business = replace_na(business, 0)) %>% 
  mutate(government = replace_na(government, 0)) %>% 
  mutate(nonprofit = replace_na(nonprofit, 0)) %>% 
  group_by(login, email, company, location) %>%
  summarise(organization = paste(organization, collapse='|'),
            academic = sum(academic), business = sum(business), 
            government = sum(government), nonprofit = sum(nonprofit))

classified_government <- classified_government %>% 
  mutate(organization = stringr::str_replace(organization, "Misc. Government\\|", ""),
         organization = stringr::str_replace(organization, "\\|Misc. Government", ""))

gov_counts <- classified_government %>% 
  group_by(organization) %>% 
  count() %>% 
  arrange(-n)


org_counts <- classified_orgs %>% 
  group_by(organization) %>% 
  count() %>% 
  arrange(-n)

leftovers <- uva_scraped_data %>% 
  filter(!(login %in% classified_orgs$login) & 
           (!is.na(company) | (!is.na(email) & 
        !grepl("gmail.com|aol.com|hotmail.com|protonmail.com|qq.com|outlook.com|live.com|naver.com", email)))) 
  
leftover_counts <- leftovers %>% 
  mutate(company = tolower(company)) %>% 
  group_by(company) %>% 
  count() %>% 
  arrange(-n)




