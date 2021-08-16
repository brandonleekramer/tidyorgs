
rm(list=ls())
library("tidyverse")
load_all()
data(github_users)

classified_by_text <- github_users %>%
  match_orgs_by_text(login, company, organization, academic)

classified_by_email <- github_users %>%
  match_orgs_by_email(login, email, organization, academic)

classified_orgs <- github_users %>%
  match_orgs_to_sector(login, company, organization, academic, email)





# https://r-pkgs.org/whole-game.html


