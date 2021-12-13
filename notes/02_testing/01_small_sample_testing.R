
rm(list=ls())
library("tidyverse")
library("tidyorgs")
library("devtools")
load_all()
data(github_users)


load_all()
classified_academic <- github_users %>%
  detect_academic(login, company, organization, email,
                  country = TRUE, parent_org = TRUE, org_type = TRUE) %>% 
  filter(academic == 1)

classified_nonprofit_2 <- github_users %>%
  detect_nonprofit(login, company, organization, email, 
                   country = TRUE, parent_org = TRUE, org_type = TRUE) %>% 
  filter(nonprofit == 1)


load_all()
classified_orgs <- github_users %>%
  detect_orgs(login, company, organization, "academic", email)

#setwd("~/Documents/git/oss-2020/data")
setwd("~/git/oss-2020/data/iariw-aea/")
uva_scraped_data <- readRDS("github_sectored_101321.rds") 
uva_scraped_data <- uva_scraped_data %>% 
  select(login, company, location, email)
setwd("~/git/tidyorgs")
classified_nonprofit <- uva_scraped_data %>%
  detect_nonprofit(login, company, organization, email, 
                   country = FALSE, parent_org = FALSE, org_type = FALSE) %>% 
  filter(nonprofit == 1)

counts <- classified_nonprofit %>% 
  group_by(organization) %>% 
  count() %>% 
  arrange(-n)


misc_data <- classified_nonprofit %>% 
  filter(organization == "Misc. Non-Profit") %>% 
  group_by(company) %>% 
  count() %>% 
  arrange(-n)



setwd("~/Documents/git/tidyorgs")
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
  detect_nonprofit(login, company, organization, email, 
                   country = FALSE, parent_org = FALSE, org_type = FALSE) %>% 
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


# aol email, 2015 inc, Hewlett-Packard|Hewlett-Packard Enterprise

library("tidyverse")
load_all()
data(github_users)
# function that combines all of the above
load_all()
classified_orgs <- github_users %>%
  detect_orgs(login, company, organization, "academic", email)

load_all()
text_to_orgs_df <- github_users %>%
  text_to_orgs(login, company, organization, "business")

load_all()
email_to_orgs_df <- github_users %>%
  email_to_orgs(login, email, organization, "government")

load_all()
email_to_sectors_df <- github_users %>%
  email_to_sectors(login, email, organization, "government")

# function that combines all of the above
classified_orgs <- github_users %>%
  detect_orgs(login, company, organization, academic, email)

# testing a different dataset now 

load_all()
setwd("~/Documents/git/oss-2020/data")
uva_scraped_data <- readRDS("github_sectored_101321.rds") 
uva_scraped_data <- uva_scraped_data %>% filter(academic == 1)
counts_by_school <- uva_scraped_data %>% 
  mutate(school = tolower(organization)) %>% 
  count(school) %>% 
  rowid_to_column() %>%
  arrange(-n) %>% 
  mutate(rowid = as.character(rowid), 
         email = "nothing@gmail.com")

# function that combines all of the above
load_all()
classified_orgs <- counts_by_school %>%
  detect_orgs(rowid, school, organization, academic) %>% 
  filter(grepl("\\|", organization) & !grepl("\\|", school))

lets_check <- uva_scraped_data %>% 
  filter(organization == "Misc. Academic")



# fixes = tons of regex work OR another clause that removes branches from roots (or vice versa)
# york and new york university 
# cal berkeley and all cal schools 
# michigan 
# École Polytechnique|École Polytechnique Fédérale
# National University|National University of Singapore
# South University of Science and Technology Of China
# Universidad Nacional|Universidad Nacional de Colo
# National University|Seoul National University
# student.adelaide.edu.au
# stu.sdu.edu.kz
# uni.sydney.edu.au


# i think the first priority is getting personal months for bayoan to get counts 
# second is integrating small protoypes from government, nonprofit, and business 
# bringing them all together will help us determine how to deal with cross-sector and cross-org conflicts (like how to prioritize bio vs email conflicts) and then we can scale from there 


# rethinking the sequence to how we match 
# currently: bio matches, bio misc, email matches, email misc 
# this can lead to inflated misc counts in the case that we do not have text col developed well enough but do have email matches (seems to rarely happen at the moment)
# proposing: bio match, email match, bio misc, email misc
# but then add a "match_by" clause that rank orders orgs by bio, email, text quality 
# this will also eventually help me deal with matching to multiple institutions and fractioning counts, which we do currently deal with an optimal way >>> i can use the same algo but use meta-data within the algo to determine (a) current vs past affiliations and (b) then fraction based upon that 

# duplicate coding = regex will not fix the issue 
# first find all of the root phrases that occur with other branch phrases 
# then create a dictionary with all root phrases of the left 
# and all of the branches to those roots on the right hand side 
# if an entry has both a root and its branch then choose its branch else its root 
# if if has any information AND misc. academic only choose other information 
# this must be done before the regex step AND it does not make the regex void 
# but in fact this could be a scalable method for creating all the regex moving forward 
# for example.... 
# university of california - university of california berkeley - YES - regex 
# university of california - university of california san diego - NO - root-branch 
# university of california - university of california berkeley - NO

# how? 
# unnest all of the catch_terms by "|" 
# if string_1 is a subset of any other string then add to col_1 and add branch to col_2
# then to automate the regex.... 
# take the branch phrases, remove their overall and collapse with " |" nested in "()"
# then take the root and create the regex attached to that 

# so the sequence is get a list > copy and lower to catch_terms 
# copy this to a second root_branch dictionary and then copy the regex to recode_col 

# the sequence really: funnel_catch, debranch, and standardize  


# -----
# we can then change the sequence to bio match, email match, bio misc, email misc
# and remove all of the filtering from the funnel matching process 
# still need to think through how to handle the misc_sectoring part but thats ok 













