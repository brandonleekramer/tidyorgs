
# load packages and data 
rm(list=ls())
library(tidyverse)

aggregated_data <- read_csv("~/git/tidyorgs/data-raw/business_data/01_wiki_data/wikidata_business_data_090821.csv")
country_list <- read_csv("~/git/tidyorgs/data-raw/business_data/01_wiki_data/countries_data.csv")
country_list <- country_list %>% 
  select(country) %>% 
  drop_na(country) %>% 
  mutate(country = str_c(" \\(", country, "\\)"))
country_list <- paste0(country_list$country, collapse = "|")

# clean the company, all_names and country column names 
clean_countries <- aggregated_data %>% 
  mutate(company = str_replace_all(company, country_list, ""),
         all_names = str_replace_all(all_names, country_list, ""),
         company = str_replace(company, " \\(Czechia\\)", ""),
         all_names = str_replace(all_names, " \\(Czechia\\)", ""),
         country = str_replace(country, "United States of America", "United States"),
         company = str_replace_all(company, ",", ""),
         company = str_replace_all(company, '\\"', ""),
         company = str_replace_all(company, '\\“', ""),
         company = str_replace_all(company, '\\”', ""),
         company = str_replace_all(company, '\\„', ""),
         company = str_replace_all(company, " - ", " "),
         company = str_replace_all(company, "\\'\\'", ""),
         company = str_replace_all(company, "1\\. ", ""),
         company = str_replace_all(company, "1\\.", ""),
         company = str_replace_all(company, "10\\. |11\\. |12\\. |13\\. |14\\. |15\\. |16\\. |1\\. |2\\. |3\\. |4\\. |5\\. |6\\. |7\\. |8\\. |9\\. ", ""),
         company = str_replace_all(company, "1\\.", ""),
         company = trimws(company),
         all_names = str_replace_all(all_names, ",", ""),
         all_names = str_replace_all(all_names, '\\"', ""),
         all_names = str_replace_all(all_names, '\\“', ""),
         all_names = str_replace_all(all_names, '\\”', ""),
         all_names = str_replace_all(all_names, '\\„', ""),
         all_names = str_replace_all(all_names, " - ", " "),
         all_names = str_replace_all(all_names, "-", " "),
         all_names = str_replace_all(all_names, "\\'\\'", ""),
         all_names = str_replace_all(all_names, "1\\. ", ""),
         all_names = str_replace_all(all_names, "1\\.", ""),
         all_names = str_replace_all(all_names, "10\\. |11\\. |12\\. |13\\. |14\\. |15\\. |16\\. |1\\. |2\\. |3\\. |4\\. |5\\. |6\\. |7\\. |8\\. |9\\. ", ""),
         all_names = str_replace_all(all_names, "1\\.", ""),
         all_names = tolower(all_names),
         all_names = str_replace_all(all_names, " inc\\.", " inc"),
         all_names = str_replace_all(all_names, " inc", " inc"),
         all_names = str_replace_all(all_names, "inc\\.", "inc"),
         all_names = str_replace_all(all_names, " llc", " llc"),
         all_names = str_replace_all(all_names, "co\\.", "co"),
         all_names = str_replace_all(all_names, "ltd\\.", "ltd"),
         all_names = str_replace_all(all_names, "corp\\.", "corp"),
         all_names = str_replace(all_names, "sp\\. z o\\.o\\.", ""),
         all_names = str_replace(all_names, "s\\.r\\.o.|s r\\.o\\.|s\\. r\\. o\\.", ""),
         all_names = str_replace(all_names, " a\\.s\\.", " as")) %>%
  separate_rows(all_names, sep="\\|") %>% 
  mutate(all_names = trimws(all_names)) %>% 
  distinct(company, all_names, country, .keep_all = TRUE) %>% 
  group_by(company, city, country, email_domain, website_url, inception_date, owned_by,
           subsidiary_to, owner_of, parent_of, orgs_in, categories_in, industry_in, products_made,
           iso_17442, duns_number, wiki_id, loc_id, msacademic_id, grid_id, opencorp_id, 
           crunchbase_id, littlesis_id, twitter_id, youtube_id, facebook_id, instagram_id, 
           subreddit_id, linkedin_id, github_id, quora_id, googlenews_id, bbcnews_id) %>% 
  mutate(all_names = paste(all_names, collapse = "|")) %>% 
  distinct_all() %>% ungroup()

# add in duplicate names 
dupe_companies <- clean_countries %>% 
  janitor::get_dupes(company) %>% 
  distinct(company)
dupe_companies <- paste(na.omit(dupe_companies$company), collapse = "|")
clean_countries <- clean_countries %>% 
  mutate(dupe_name = str_detect(company, dupe_companies)) 

# then join all of the tech data 
tidyorgs_111021 <- read_csv("~/git/tidyorgs/data-raw/business_data/03_github_data/tidyorgs_business_111021.csv")
tech_companies_data_090821 <- read_csv("~/git/tidyorgs/data-raw/business_data/01_wiki_data/tech_companies_data_090821.csv")
tech_companies_data_090821 <- tech_companies_data_090821 %>% select(-country)
tech_data_joined <- tidyorgs_111021 %>% 
  select(-industry_in) %>% 
  left_join(tech_companies_data_090821, by = c("organization_name" = "company"))
tech_filtered <- tech_data_joined %>% 
  filter(!(organization_name %in% clean_countries$company))
tech_filtered <- tech_filtered %>% 
  filter(!(domains %in% clean_countries$email_domain))
# only a very small bit of data that is unique so i obviously included that tech query in the broader query (good!)

# now lets add the tech_filtered data into the original dataset. to do that, we need the same col names 
data_to_bind <- clean_countries %>% 
  rename(catch_terms = all_names,
         domains = email_domain,
         organization_name = company, 
         parent_org = subsidiary_to) %>% 
  mutate(recode_column = catch_terms,
         dupe_name = as.character(dupe_name)) %>% 
  select(catch_terms, recode_column, organization_name, dupe_name, city, country, parent_org, everything()) %>% 
  mutate(datasource = "Wikidata Query 2021-09-08")

tech_to_bind <- tech_filtered %>% 
  mutate(dupe_name = "MAYBE") %>% 
  select(catch_terms, recode_column, organization_name, dupe_name, city, country, parent_org, domains, 
         website_url, inception_date, owned_by, owner_of, parent_of, orgs_in, categories_in, industry_in, 
         products_made, iso_17442, duns_number, wiki_id, loc_id, msacademic_id, grid_id, opencorp_id, 
         crunchbase_id, littlesis_id, twitter_id, youtube_id, facebook_id, instagram_id, 
         subreddit_id, linkedin_id, github_id, quora_id, googlenews_id, bbcnews_id) %>% 
  mutate("GitHub Data 2021-11-10")

data_to_bind <- data_to_bind %>% 
  bind_rows(datasource = tech_to_bind) %>% 
  arrange(organization_name)

# adding in SEC data that neil and aritra scraped next (see /business_data/02_sec_data/02_foreign_businesses.R)

sec_data_111021 <- read_csv("~/git/tidyorgs/data-raw/business_data/03_github_data/sec_aggregated_111021.csv")












# need to figure out dots and variations for foreign businesses
# a.s.|o.p.f = other business nicknames 
# deduplicate all_names column by unnesting and deduplicating 

         # consolidate = 3m, 10x genomics, 451 research, 5n plus 

# will need editing: Access (One), Atari (Corporation), CAST (AI), Cision, Facebook, Sony 
# asian oceanian coputing industry organization
















