
rm(list=ls())
library("tidyverse")
library("tidyorgs")
load_all()
data(github_users)
data("academic_institutions")

library("tidyverse")
load_all()
data(github_users)
# function that combines all of the above
classified_orgs <- github_users %>%
  detect_orgs(login, company, organization, academic, email, parent_org = TRUE)

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










