
rm(list=ls())
library("tidyverse")
library("tidyorgs")
load_all()
data(github_users)

# orgs
text_to_orgs_df <- github_users %>%
  text_to_orgs(login, company, organization, academic)

text_to_sectors_df <- github_users %>%
  text_to_sectors(login, company, organization, academic)

email_to_orgs_df <- github_users %>%
  email_to_orgs(login, email, organization, academic)

email_to_sectors_df <- github_users %>%
  email_to_sectors(login, email, organization, academic)

# function that combines all of the above
classified_orgs <- github_users %>%
  detect_orgs(login, company, organization, academic, email)

# countries 
text_to_countries_df <- github_users %>%
  text_to_countries(login, location, country_name)

email_to_countries_df <- github_users %>%
  email_to_countries(login, email, country_name)

# function that combines all of the above
detected_countries_df <- github_users %>% 
  detect_countries(login, location, country_name, email) 



# https://r-pkgs.org/whole-game.html


