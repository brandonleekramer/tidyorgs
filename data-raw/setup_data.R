# code for preparing internal and external datasets

library(dplyr)
library(RPostgreSQL)

# academic institutions data ---------------------------------------------

academic_instiutions <- readr::read_csv("data-raw/academic_institutions.csv")

readr::write_rds(academic_instiutions, "R/academic_instiutions.rds")
usethis::use_data(academic_instiutions, overwrite = TRUE)

# misc. sector terms -----------------------------------------------------

sector_terms <- readr::read_csv("data-raw/sector_terms.csv")

readr::write_rds(sector_terms, "R/sector_terms.rds")
usethis::use_data(sector_terms, overwrite = TRUE)

# misc. sector domains ---------------------------------------------------

sector_domains <- readr::read_csv("data-raw/sector_domains.csv")

readr::write_rds(sector_domains, "R/sector_domains.rds")
usethis::use_data(sector_domains, overwrite = TRUE)

# github_users data ------------------------------------------------------

conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
github_users <- dbGetQuery(conn, "SELECT login, company, email FROM gh.ctrs_extra
                                  WHERE company IS NOT NULL AND email IS NOT NULL LIMIT 500")
dbDisconnect(conn)

github_users <- github_users %>%
  mutate(email = tolower(email)) %>%
  mutate(email = sub('.*@', '', email)) %>%
  mutate(email = paste0("username@", email))

readr::write_csv(github_users, "data-raw/github_users.csv")
usethis::use_data(github_users, overwrite = TRUE)






