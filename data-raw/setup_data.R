# academic institutions data ---------------------------------------------

library(dplyr)
academic_institutions <- readr::read_csv("data-raw/academic_institutions.csv")
#readr::write_rds(academic_institutions, "R/academic_institutions.rds")
#usethis::use_data(academic_institutions, overwrite = TRUE)
usethis::use_data(academic_institutions, internal = TRUE, overwrite = TRUE)

# misc. sector terms -----------------------------------------------------

library(dplyr)
sector_terms <- readr::read_csv("data-raw/sector_terms.csv")
#readr::write_rds(sector_terms, "R/sector_terms.rds")
#usethis::use_data(sector_terms, overwrite = TRUE)
usethis::use_data(sector_terms, internal = TRUE, overwrite = TRUE)

# misc. sector domains ---------------------------------------------------

library(dplyr)
sector_domains <- readr::read_csv("data-raw/sector_domains.csv")
#readr::write_rds(sector_domains, "R/sector_domains.rds")
#usethis::use_data(sector_domains, overwrite = TRUE)
usethis::use_data(sector_domains, internal = TRUE, overwrite = TRUE)

# countries data ---------------------------------------------

library(tidyverse)
world_cities <- read_csv("data-raw/worldcities_raw.csv")
world_cities_edited <- world_cities %>% 
  arrange(country) %>% 
  mutate(city = tolower(city), 
         city_ascii = tolower(city_ascii)) %>% 
  unite("city_combined", c("city", "city_ascii"), sep = "|") %>% 
  mutate(city_combined = str_replace(city_combined, "\\.", " ")) %>% 
  select(city_combined, country) %>% 
  separate_rows(city_combined, sep = "\\|") %>%
  distinct(city_combined, country) %>% 
  group_by(country) %>% 
  mutate(city_combined = paste(city_combined, collapse = "|")) %>% 
  distinct(city_combined, country)
readr::write_csv(world_cities_edited, "data-raw/worldcities_collapsed.csv")

library(dplyr)
countries_data <- readr::read_csv("data-raw/countries_data.csv")
readr::write_rds(countries_data, "R/countries_data.rds")
usethis::use_data(countries_data, overwrite = TRUE)
usethis::use_data(countries_data, internal = TRUE, overwrite = TRUE)

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






