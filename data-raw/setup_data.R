# academic institutions data ---------------------------------------------

library(dplyr)
academic_institutions <- readr::read_csv("data-raw/academic_institutions.csv")
#readr::write_rds(academic_institutions, "R/academic_institutions.rds")
usethis::use_data(academic_institutions, overwrite = TRUE)
usethis::use_data(academic_institutions, internal = TRUE, overwrite = TRUE)

# business institutions data ---------------------------------------------------

library(dplyr)
business_data <- readr::read_csv("data-raw/business_data/tech_companies_data_090821.csv")
business_data <- business_data %>% 
  select(company, email_domain, country, subsidiary_to, industry_in) %>% 
  rename(organization_name = company, domains = email_domain, 
         parent_org = subsidiary_to, org_type = industry_in)
business_data <- business_data %>% 
  mutate(catch_terms = tolower(organization_name),
         catch_terms = str_replace_all(catch_terms, ",|\\.|\\.com| inc| llc", ""),
         catch_terms = str_replace_all(catch_terms, "\\-", " "),
         catch_terms = str_replace_all(catch_terms, " \\(united states\\)", ""),
         catch_terms = trimws(catch_terms),
         recode_column = catch_terms) %>% 
  select(catch_terms, recode_column, everything())
#write_csv(business_data, "data-raw/business_data/tech_cleaned_102521.csv")

business_data <- readr::read_csv("data-raw/business_data.csv")
usethis::use_data(business_data, overwrite = TRUE)
usethis::use_data(business_data, internal = TRUE, overwrite = TRUE)

# nonprofit data ---------------------------------------------

library(dplyr)
nonprofit_data <- readr::read_csv("data-raw/nonprofit_data.csv")
usethis::use_data(nonprofit_data, overwrite = TRUE)
usethis::use_data(nonprofit_data, internal = TRUE, overwrite = TRUE)

# government data ---------------------------------------------

library(dplyr)
government_data <- readr::read_csv("data-raw/government_data.csv")
usethis::use_data(government_data, overwrite = TRUE)
usethis::use_data(government_data, internal = TRUE, overwrite = TRUE)

# misc. sector terms -----------------------------------------------------

library(dplyr)
sector_terms <- readr::read_csv("data-raw/sector_terms.csv")
usethis::use_data(sector_terms, overwrite = TRUE)
usethis::use_data(sector_terms, internal = TRUE, overwrite = TRUE)

# misc. sector domains ---------------------------------------------------

library(dplyr)
sector_domains <- readr::read_csv("data-raw/sector_domains.csv")
#readr::write_rds(sector_domains, "R/sector_domains.rds")
usethis::use_data(sector_domains, overwrite = TRUE)
usethis::use_data(sector_domains, internal = TRUE, overwrite = TRUE)

# github_users data ------------------------------------------------------

library(RPostgreSQL)
library(dplyr)
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
github_users <- dbGetQuery(conn, "SELECT login, email, company, location FROM gh.ctrs_extra
                                  WHERE company IS NOT NULL AND email IS NOT NULL AND location IS NOT NULL LIMIT 500")
dbDisconnect(conn)

github_users <- github_users %>%
  mutate(email = tolower(email)) %>%
  mutate(email = sub('.*@', '', email)) %>%
  mutate(email = paste0("username@", email))

readr::write_csv(github_users, "data-raw/github_users.csv")
usethis::use_data(github_users, overwrite = TRUE)






