
rm(list=ls())
library("tidyverse")
load_all()
data(github_users)

text_to_orgs_df <- github_users %>%
  text_to_orgs(login, company, organization, academic)

text_to_sectors_df <- github_users %>%
  text_to_sectors(login, company, organization, academic)

email_to_orgs_df <- github_users %>%
  email_to_orgs(login, email, organization, academic)

email_to_sectors_df <- github_users %>%
  email_to_sectors(login, email, organization, academic)

classified_orgs <- github_users %>%
  detect_orgs(login, company, organization, academic, email)





# https://r-pkgs.org/whole-game.html


