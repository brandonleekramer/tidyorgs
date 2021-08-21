

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

# 20002 
misc_academic <- academic_users %>% 
  filter(organization == "misc. academic")

  
  
  
  