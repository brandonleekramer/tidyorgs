rm(list=ls())
library(tidyverse)

setwd("~/git/tidyorgs/data-raw/wikidata/")
df_1 <- read_csv("01_business_names.csv") 
df_1 <- read_csv("19_enterprise_01.csv") %>% 
  select(business, businessLabel, officialnameLabel, shortnameLabel) %>% 
  bind_rows(df_1) %>% distinct(business, businessLabel, officialnameLabel, shortnameLabel)
df_1 <- read_csv("26_pubcomp_01.csv") %>% 
  select(business, businessLabel, officialnameLabel, shortnameLabel) %>% 
  bind_rows(df_1) %>% distinct(business, businessLabel, officialnameLabel, shortnameLabel)
df_2 <- read_csv("02_business_location.csv") 
df_2 <- read_csv("19_enterprise_01.csv") %>% 
  select(business, locationLabel, countryLabel) %>% 
  bind_rows(df_2) %>% distinct(business, locationLabel, countryLabel)
df_2 <- read_csv("26_pubcomp_01.csv") %>% 
  select(business, locationLabel, countryLabel) %>% 
  bind_rows(df_2) %>% distinct(business, locationLabel, countryLabel)

# clean those with labels 
with_labels_all_names <- df_1 %>%
  filter(!grepl("^Q[0-9]", businessLabel)) %>% 
  filter(!is.na(officialnameLabel) | !is.na(shortnameLabel)) %>% 
  unite("all_names", c("businessLabel", "officialnameLabel", "shortnameLabel"), sep = "|") %>% 
  mutate(all_names = str_replace(all_names, "NA\\|NA", ""),
         all_names = str_replace(all_names, "NA\\|", ""),
         all_names = str_replace(all_names, "\\|NA", "")) %>% 
  separate_rows(all_names, sep = "\\|") %>% 
  distinct(business, all_names) %>% 
  group_by(business) %>% 
  mutate(all_names = paste0(all_names, collapse = "|")) %>% 
  distinct(business, all_names)

with_labels <- df_1 %>%
  filter(!grepl("^Q[0-9]", businessLabel)) %>% 
  drop_na(businessLabel) %>% 
  distinct(business, businessLabel) %>%
  mutate(wiki_id = str_replace(business, "http://www.wikidata.org/entity/", "")) %>% 
  arrange(business) %>% 
  rename(company = businessLabel) %>% 
  select(wiki_id, company, business) %>% 
  left_join(with_labels_all_names, by = "business")

# clean those without labels 
names_data <- df_1 %>%
  filter(grepl("^Q[0-9]", businessLabel)) %>% 
  rename(wiki_id = businessLabel) %>% 
  filter(!is.na(officialnameLabel) | !is.na(shortnameLabel)) %>%
  unite("all_names", c("officialnameLabel", "shortnameLabel"), sep = "|") %>% 
  mutate(all_names = str_replace(all_names, "NA\\|NA", ""),
         all_names = str_replace(all_names, "NA\\|", ""),
         all_names = str_replace(all_names, "\\|NA", "")) %>% 
  distinct(business, wiki_id, all_names) %>% 
  group_by(business, wiki_id) %>% 
  mutate(all_names = paste0(all_names, collapse = "|")) %>% 
  distinct(business, wiki_id, all_names) %>% 
  mutate(company = sub("\\|.*", "", all_names)) %>% 
  select(wiki_id, company, all_names, business) %>% 
  bind_rows(with_labels)
rm(with_labels, with_labels_all_names)

city_data <- df_2 %>%
  rename(city = locationLabel) %>% 
  drop_na(city) %>% 
  distinct(business, city) %>% 
  group_by(business) %>% 
  mutate(city = paste0(city, collapse = "|")) %>% 
  distinct(business, city)

country_data <- df_2 %>%
  rename(country = countryLabel) %>% 
  drop_na(country) %>% 
  distinct(business, country) %>% 
  group_by(business) %>% 
  mutate(country = paste0(country, collapse = "|")) %>% 
  full_join(city_data, by = "business") %>% 
  distinct(business, city, country)

location_data <- names_data %>% 
  left_join(country_data, by = "business") %>% 
  select(everything(), business)
rm(df_1, df_2, city_data, country_data)

# iso 17442
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("03_business_legal.csv") %>% select(business, legalidLabel)
df_3 <- read_csv("19_enterprise_01.csv") %>% select(business, legalidLabel) %>% bind_rows(df_3)
df_3 <- read_csv("27_pubcomp_02.csv") %>% select(business, legalidLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(iso_17442 = legalidLabel) %>%
  drop_na(iso_17442) %>%
  distinct(business, iso_17442) %>% 
  group_by(business) %>% 
  mutate(iso_17442 = paste0(iso_17442, collapse = "|")) %>% 
  distinct(business, iso_17442)
business_data <- location_data %>% 
  left_join(df_3, by = "business")

# duns_id 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("04_business_extra.csv") %>% select(business, dunsLabel)
df_3 <- read_csv("19_enterprise_01.csv") %>% select(business, dunsLabel) %>% bind_rows(df_3)
df_3 <- read_csv("27_pubcomp_02.csv") %>% select(business, dunsLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(duns_number = dunsLabel) %>%
  drop_na(duns_number) %>%
  distinct(business, duns_number) %>% 
  group_by(business) %>% 
  mutate(duns_number = paste0(duns_number, collapse = "|")) %>% 
  distinct(business, duns_number)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# duns_id 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("04_business_extra.csv") %>% select(business, inceptionLabel)
df_3 <- read_csv("19_enterprise_01.csv") %>% select(business, inceptionLabel) %>% bind_rows(df_3)
df_3 <- read_csv("27_pubcomp_02.csv") %>% select(business, inceptionLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(inception_date = inceptionLabel) %>%
  drop_na(inception_date) %>%
  distinct(business, inception_date) %>% 
  group_by(business) %>% 
  mutate(inception_date = paste0(inception_date, collapse = "|")) %>% 
  distinct(business, inception_date)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# parent_data 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("05_business_parent.csv") %>% select(business, ownedbyLabel)
df_3 <- read_csv("20_enterprise_02.csv") %>% select(business, ownedbyLabel) %>% bind_rows(df_3)
df_3 <- read_csv("28_pubcomp_03.csv") %>% select(business, ownedbyLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(owned_by = ownedbyLabel) %>%
  drop_na(owned_by) %>%
  distinct(business, owned_by) %>% 
  group_by(business) %>% 
  mutate(owned_by = paste0(owned_by, collapse = "|")) %>% 
  distinct(business, owned_by)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# parent_org 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("05_business_parent.csv") %>% select(business, parentorgLabel)
df_3 <- read_csv("20_enterprise_02.csv") %>% select(business, parentorgLabel) %>% bind_rows(df_3)
df_3 <- read_csv("28_pubcomp_03.csv") %>% select(business, parentorgLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(parent_org = parentorgLabel) %>% # later changed to subsidiary_to
  drop_na(parent_org) %>%
  distinct(business, parent_org) %>% 
  group_by(business) %>% 
  mutate(parent_org = paste0(parent_org, collapse = "|")) %>% 
  distinct(business, parent_org)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# part_of (stock, exchanges, big tech, etc)
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("06_business_children.csv") %>% select(business, partofLabel)
df_3 <- read_csv("20_enterprise_02.csv") %>% select(business, partofLabel) %>% bind_rows(df_3)
df_3 <- read_csv("28_pubcomp_03.csv") %>% select(business, partofLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(part_of = partofLabel) %>%
  drop_na(part_of) %>%
  distinct(business, part_of) %>% 
  group_by(business) %>% 
  mutate(part_of = paste0(part_of, collapse = "|")) %>% 
  distinct(business, part_of)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# affiliated_orgs 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("06_business_children.csv") %>% select(business, memberofLabel)
df_3 <- read_csv("20_enterprise_02.csv") %>% select(business, memberofLabel) %>% bind_rows(df_3)
df_3 <- read_csv("28_pubcomp_03.csv") %>% select(business, memberofLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(affiliated_orgs = memberofLabel) %>%
  drop_na(affiliated_orgs) %>%
  distinct(business, affiliated_orgs) %>% 
  group_by(business) %>% 
  mutate(affiliated_orgs = paste0(affiliated_orgs, collapse = "|")) %>% 
  distinct(business, affiliated_orgs)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# owner_of 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("07_business_children2.csv") %>% select(business, ownerofLabel)
df_3 <- read_csv("20_enterprise_02.csv") %>% select(business, ownerofLabel) %>% bind_rows(df_3)
df_3 <- read_csv("29_pubcomp_04.csv") %>% select(business, ownerofLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(owner_of = ownerofLabel) %>%
  drop_na(owner_of) %>%
  distinct(business, owner_of) %>% 
  group_by(business) %>% 
  mutate(owner_of = paste0(owner_of, collapse = "|")) %>% 
  distinct(business, owner_of)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("07_business_children2.csv") %>% select(business, subsidiaryLabel)
df_3 <- read_csv("21_enterprise_03.csv") %>% select(business, subsidiaryLabel) %>% bind_rows(df_3)
df_3 <- read_csv("29_pubcomp_04.csv") %>% select(business, subsidiaryLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(parent_of = subsidiaryLabel) %>%
  drop_na(parent_of) %>%
  distinct(business, parent_of) %>% 
  group_by(business) %>% 
  mutate(parent_of = paste0(parent_of, collapse = "|")) %>% 
  distinct(business, parent_of)
business_data <- business_data %>% 
  left_join(df_3, by = "business") %>% 
  rename(subsidiary_to = parent_org, 
         orgs_in = affiliated_orgs, 
         categories_in = part_of)

business_data <- business_data %>% 
  select(wiki_id, company, all_names, city, country, inception_date, iso_17442, duns_number, 
         owned_by, subsidiary_to, owner_of, parent_of, orgs_in, categories_in)

# industry_in
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("08_business_products.csv") %>% select(business, industryLabel)
df_3 <- read_csv("21_enterprise_03.csv") %>% select(business, industryLabel) %>% bind_rows(df_3)
df_3 <- read_csv("29_pubcomp_04.csv") %>% select(business, industryLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(industry_in = industryLabel) %>%
  drop_na(industry_in) %>%
  distinct(business, industry_in) %>% 
  group_by(business) %>% 
  mutate(industry_in = paste0(industry_in, collapse = "|")) %>% 
  distinct(business, industry_in)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# products_made
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("08_business_products.csv") %>% select(business, productsLabel)
df_3 <- read_csv("21_enterprise_03.csv") %>% select(business, productsLabel) %>% bind_rows(df_3)
df_3 <- read_csv("30_pubcomp_05.csv") %>% select(business, productsLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(products_made = productsLabel) %>%
  drop_na(products_made) %>%
  distinct(business, products_made) %>% 
  group_by(business) %>% 
  mutate(products_made = paste0(products_made, collapse = "|")) %>% 
  distinct(business, products_made)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# website 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("09_business_domains.csv") %>% select(business, websiteLabel)
df_3 <- read_csv("21_enterprise_03.csv") %>% select(business, websiteLabel) %>% bind_rows(df_3)
df_3 <- read_csv("30_pubcomp_05.csv") %>% select(business, websiteLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(website_url = websiteLabel) %>%
  drop_na(website_url) %>%
  distinct(business, website_url) %>% 
  group_by(business) %>% 
  mutate(website_url = paste0(website_url, collapse = "|")) %>% 
  distinct(business, website_url)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# email 
email_data <- df_3 %>% 
  separate_rows(website, sep = "\\|") %>% 
  mutate(domains = tolower(website),
         domains = sub('https://', '', domains),
         domains = sub('http://', '', domains),
         domains = sub('www[0-9].', '', domains),
         domains = sub('www-01.', '', domains),
         domains = sub('www[5a|5f|7a|10|15|22].', '', domains),
         domains = sub('wwww.', '', domains),
         domains = sub('wwws.', '', domains),
         domains = sub('www.', '', domains),
         domains = sub('/$', '', domains),
         domains = sub('/.*', '', domains)) %>% 
  filter(!grepl("^\\.", domains)) %>% 
  rename(email = domains) %>% 
  distinct(business, email) %>% 
  group_by(business) %>% 
  mutate(email = paste0(email, collapse = "|")) %>% 
  distinct(business, email)
business_data <- business_data %>% 
  left_join(email_data, by = "business")


# googlenewsLabel 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("10_business_news.csv") %>% select(business, googlenewsLabel)
df_3 <- read_csv("22_enterprise_04.csv") %>% select(business, googlenewsLabel) %>% bind_rows(df_3)
df_3 <- read_csv("31_pubcomp_06.csv") %>% select(business, googlenewsLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(google_news = googlenewsLabel) %>%
  drop_na(google_news) %>%
  distinct(business, google_news) %>% 
  group_by(business) %>% 
  mutate(google_news = paste0(google_news, collapse = "|")) %>% 
  distinct(business, google_news)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# googlenewsLabel 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("10_business_news.csv") %>% select(business, bbcnewsLabel)
df_3 <- read_csv("22_enterprise_04.csv") %>% select(business, bbcnewsLabel) %>% bind_rows(df_3)
df_3 <- read_csv("31_pubcomp_06.csv") %>% select(business, bbcnewsLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(bbc_news = bbcnewsLabel) %>%
  drop_na(bbc_news) %>%
  distinct(business, bbc_news) %>% 
  group_by(business) %>% 
  mutate(bbc_news = paste0(bbc_news, collapse = "|")) %>% 
  distinct(business, bbc_news)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# library of congress id  
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("11_business_ticker.csv") %>% select(business, libofcongressLabel)
df_3 <- read_csv("22_enterprise_04.csv") %>% select(business, libofcongressLabel) %>% bind_rows(df_3)
df_3 <- read_csv("31_pubcomp_06.csv") %>% select(business, libofcongressLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(loc_id = libofcongressLabel) %>%
  drop_na(loc_id) %>%
  distinct(business, loc_id) %>% 
  group_by(business) %>% 
  mutate(loc_id = paste0(loc_id, collapse = "|")) %>% 
  distinct(business, loc_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# library of congress id  
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("13_business_corporate.csv") %>% select(business, libofcongressLabel)
df_3 <- read_csv("22_enterprise_04.csv") %>% select(business, libofcongressLabel) %>% bind_rows(df_3)
df_3 <- read_csv("31_pubcomp_06.csv") %>% select(business, libofcongressLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(loc_id = libofcongressLabel) %>%
  drop_na(loc_id) %>%
  distinct(business, loc_id) %>% 
  group_by(business) %>% 
  mutate(loc_id = paste0(loc_id, collapse = "|")) %>% 
  distinct(business, loc_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

write_csv(business_data, "~/git/tidyorgs/data-raw/wikidata/joined_data.csv")
business_data <- read_csv("~/git/tidyorgs/data-raw/wikidata/joined_data.csv")
# going to skip some of these variables 

#ms academic 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("12_business_academic.csv") %>% select(business, msacademicLabel)
df_3 <- read_csv("22_enterprise_04.csv") %>% select(business, msacademicLabel) %>% bind_rows(df_3)
df_3 <- read_csv("32_pubcomp_07.csv") %>% select(business, msacademicLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(msacademic_id = msacademicLabel) %>%
  drop_na(msacademic_id) %>%
  distinct(business, msacademic_id) %>% 
  group_by(business) %>% 
  mutate(msacademic_id = paste0(msacademic_id, collapse = "|")) %>% 
  distinct(business, msacademic_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#grid id  
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("12_business_academic.csv") %>% select(business, grididLabel)
df_3 <- read_csv("23_enterprise_05.csv") %>% select(business, grididLabel) %>% bind_rows(df_3)
df_3 <- read_csv("32_pubcomp_07.csv") %>% select(business, grididLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(grid_id = grididLabel) %>%
  drop_na(grid_id) %>%
  distinct(business, grid_id) %>% 
  group_by(business) %>% 
  mutate(grid_id = paste0(grid_id, collapse = "|")) %>% 
  distinct(business, grid_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#opencorporatesLabel 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("13_business_corporate.csv") %>% select(business, opencorporatesLabel)
df_3 <- read_csv("23_enterprise_05.csv") %>% select(business, opencorporatesLabel) %>% bind_rows(df_3)
df_3 <- read_csv("32_pubcomp_07.csv") %>% select(business, opencorporatesLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(opencorp_id = opencorporatesLabel) %>%
  drop_na(opencorp_id) %>%
  distinct(business, opencorp_id) %>% 
  group_by(business) %>% 
  mutate(opencorp_id = paste0(opencorp_id, collapse = "|")) %>% 
  distinct(business, opencorp_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#crunchbaseLabel 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("13_business_corporate.csv") %>% select(business, crunchbaseLabel)
df_3 <- read_csv("23_enterprise_05.csv") %>% select(business, crunchbaseLabel) %>% bind_rows(df_3)
df_3 <- read_csv("33_pubcomp_08.csv") %>% select(business, crunchbaseLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(crunchbase_id = crunchbaseLabel) %>%
  drop_na(crunchbase_id) %>%
  distinct(business, crunchbase_id) %>% 
  group_by(business) %>% 
  mutate(crunchbase_id = paste0(crunchbase_id, collapse = "|")) %>% 
  distinct(business, crunchbase_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#littlesisLabel 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("14_business_quoralilsis.csv") %>% select(business, littlesisLabel)
df_3 <- read_csv("23_enterprise_05.csv") %>% select(business, littlesisLabel) %>% bind_rows(df_3)
df_3 <- read_csv("33_pubcomp_08.csv") %>% select(business, littlesisLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(littlesis_id = littlesisLabel) %>%
  drop_na(littlesis_id) %>%
  distinct(business, littlesis_id) %>% 
  group_by(business) %>% 
  mutate(littlesis_id = paste0(littlesis_id, collapse = "|")) %>% 
  distinct(business, littlesis_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#quoraLabel 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("14_business_quoralilsis.csv") %>% select(business, quoraLabel)
df_3 <- read_csv("23_enterprise_05.csv") %>% select(business, quoraLabel) %>% bind_rows(df_3)
df_3 <- read_csv("33_pubcomp_08.csv") %>% select(business, quoraLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(quora_id = quoraLabel) %>%
  drop_na(quora_id) %>%
  distinct(business, quora_id) %>% 
  group_by(business) %>% 
  mutate(quora_id = paste0(quora_id, collapse = "|")) %>% 
  distinct(business, quora_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#twitter 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("15_business_twitter.csv") %>% select(business, twitterLabel)
df_3 <- read_csv("24_enterprise_06.csv") %>% select(business, twitterLabel) %>% bind_rows(df_3)
df_3 <- read_csv("34_pubcomp_09.csv") %>% select(business, twitterLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(twitter_id = twitterLabel) %>%
  drop_na(twitter_id) %>%
  distinct(business, twitter_id) %>% 
  group_by(business) %>% 
  mutate(twitter_id = paste0(twitter_id, collapse = "|")) %>% 
  distinct(business, twitter_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#youtube 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("15_business_twitter.csv") %>% select(business, youtubeLabel)
df_3 <- read_csv("24_enterprise_06.csv") %>% select(business, youtubeLabel) %>% bind_rows(df_3)
df_3 <- read_csv("34_pubcomp_09.csv") %>% select(business, youtubeLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(youtube_id = youtubeLabel) %>%
  drop_na(youtube_id) %>%
  distinct(business, youtube_id) %>% 
  group_by(business) %>% 
  mutate(youtube_id = paste0(youtube_id, collapse = "|")) %>% 
  distinct(business, youtube_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#facebook 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("16_business_facebook.csv") %>% select(business, facebookLabel)
df_3 <- read_csv("24_enterprise_06.csv") %>% select(business, facebookLabel) %>% bind_rows(df_3)
df_3 <- read_csv("34_pubcomp_09.csv") %>% select(business, facebookLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(facebook_id = facebookLabel) %>%
  drop_na(facebook_id) %>%
  distinct(business, facebook_id) %>% 
  group_by(business) %>% 
  mutate(facebook_id = paste0(facebook_id, collapse = "|")) %>% 
  distinct(business, facebook_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#instagram 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("16_business_facebook.csv") %>% select(business, instagramLabel)
df_3 <- read_csv("24_enterprise_06.csv") %>% select(business, instagramLabel) %>% bind_rows(df_3)
df_3 <- read_csv("34_pubcomp_09.csv") %>% select(business, instagramLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(instagram_id = instagramLabel) %>%
  drop_na(instagram_id) %>%
  distinct(business, instagram_id) %>% 
  group_by(business) %>% 
  mutate(instagram_id = paste0(instagram_id, collapse = "|")) %>% 
  distinct(business, instagram_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#subreddit 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("17_business_reddit.csv") %>% select(business, redditLabel)
df_3 <- read_csv("25_enterprise_07.csv") %>% select(business, redditLabel) %>% bind_rows(df_3)
df_3 <- read_csv("35_pubcomp_10.csv") %>% select(business, redditLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(subreddit_id = redditLabel) %>%
  drop_na(subreddit_id) %>%
  distinct(business, subreddit_id) %>% 
  group_by(business) %>% 
  mutate(subreddit_id = paste0(subreddit_id, collapse = "|")) %>% 
  distinct(business, subreddit_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

#linked_in 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("17_business_reddit.csv") %>% select(business, linkedinLabel)
df_3 <- read_csv("25_enterprise_07.csv") %>% select(business, linkedinLabel) %>% bind_rows(df_3)
df_3 <- read_csv("35_pubcomp_10.csv") %>% select(business, linkedinLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(linkedin_id = linkedinLabel) %>%
  drop_na(linkedin_id) %>%
  distinct(business, linkedin_id) %>% 
  group_by(business) %>% 
  mutate(linkedin_id = paste0(linkedin_id, collapse = "|")) %>% 
  distinct(business, linkedin_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# github 
setwd("~/git/tidyorgs/data-raw/wikidata/")
df_3 <- read_csv("18_business_github.csv") %>% select(business, githubLabel)
df_3 <- read_csv("25_enterprise_07.csv") %>% select(business, githubLabel) %>% bind_rows(df_3)
df_3 <- read_csv("35_pubcomp_10.csv") %>% select(business, githubLabel) %>% bind_rows(df_3)
df_3 <- df_3 %>% 
  rename(github_id = githubLabel) %>%
  drop_na(github_id) %>%
  distinct(business, github_id) %>% 
  group_by(business) %>% 
  mutate(github_id = paste0(github_id, collapse = "|")) %>% 
  distinct(business, github_id)
business_data <- business_data %>% 
  left_join(df_3, by = "business")

# ticker data was garbage = have to find this a different way 

# github topic skipped 

ordered_data <- business_data %>% 
  rename(email_domain = email,
         googlenews_id = google_news,
         bbcnews_id = bbc_news,
         alt_names = all_names) %>% 
  select(
    company, alt_names, city, country, email_domain, website_url, inception_date, 
    owned_by:products_made, iso_17442, duns_number, wiki_id,
    loc_id:littlesis_id, twitter_id:github_id, quora_id, googlenews_id, bbcnews_id
  ) %>% arrange(company) %>% 
  unite("all_names", c("company", "alt_names"), sep = "|", remove = FALSE) %>% 
  mutate(all_names = str_replace(all_names, "NA\\|NA", ""),
         all_names = str_replace(all_names, "NA\\|", ""),
         all_names = str_replace(all_names, "\\|NA", "")) %>% 
  separate_rows(all_names, sep = "\\|") %>% 
  distinct(wiki_id, all_names, .keep_all = TRUE) %>% 
  group_by(wiki_id) %>% 
  mutate(all_names = paste0(all_names, collapse = "|")) %>% 
  distinct(wiki_id, all_names, .keep_all = TRUE) %>% 
  select(company, all_names, everything(), -alt_names)

write_csv(ordered_data, "~/git/tidyorgs/data-raw/wikidata_business_data_090821.csv")

# could add state to location cols 
# need to decide what to do with duplicates from different countries (facebook, 3m, etc)
# similar issue with regional companies?? (e.g. Affiliated Foods Midwest)
# need to check for duplicate email domains (facebook.com, etc)
# need to clean up location/country column (do this with detect_countries)
# relating to companies in multiple countries in the company col, we need to decide what country 
# each row should be applied to if they have multiple countries in the location/country col
# might need to release/include this dataset in steps 
# may need to add an employees column to be pragmatic about the cleaning process 
# could also do this by industry instead (starting with big tech and food)
# could join the forbes data to this 
# want to join stock/ticker data to this once standardization is better 

rm(list=ls())
library(tidyverse)
ordered_data <- read_csv("~/git/tidyorgs/data-raw/business_data/wikidata_business_data_090821.csv")

#tech_companies <- ordered_data %>% 
#  filter(grepl("software|tech|video game", industry_in))

chk <- ordered_data %>% 
  mutate(company = str_replace(company, " \\(United States\\)", ""),
         all_names = str_replace(all_names, " \\(United States\\)", ""),
         company = str_replace(company, " \\(China\\)", ""),
         all_names = str_replace(all_names, " \\(China\\)", ""),
         company = str_replace(company, " \\(France\\)", ""),
         all_names = str_replace(all_names, " \\(France\\)", ""),
         company = str_replace(company, " \\(Czechia\\)", ""),
         all_names = str_replace(all_names, " \\(Czechia\\)", ""),
         company = str_replace(company, " \\(company\\)", ""),
         all_names = str_replace(all_names, " \\(company\\)", ""),
         company = str_replace(company, "AMRO BIOTECH PLC", "Amro Biotech PLC"),
         company = str_replace_all(company, '\\"', ""),
         company = str_replace_all(company, '\\“', ""),
         company = str_replace_all(company, '\\”', ""),
         company = str_replace_all(company, '\\„', ""),
         all_names = tolower(all_names),
         all_names = str_replace_all(all_names, ",", ""),
         all_names = str_replace_all(all_names, '\\"', ""),
         all_names = str_replace_all(all_names, '\\“', ""),
         all_names = str_replace_all(all_names, '\\”', ""),
         all_names = str_replace_all(all_names, '\\„', ""),
         all_names = str_replace_all(all_names, "-", " "),
         all_names = str_replace_all(all_names, "\\'\\'", ""),
         all_names = str_replace_all(all_names, " inc\\.", " inc"),
         all_names = str_replace_all(all_names, " inc", " inc"),
         all_names = str_replace_all(all_names, "inc\\.", "inc"),
         all_names = str_replace_all(all_names, " llc", " llc"),
         all_names = str_replace_all(all_names, "co\\.", "co"),
         all_names = str_replace_all(all_names, "ltd\\.", "ltd"),
         all_names = str_replace_all(all_names, "corp\\.", "corp"),
         all_names = str_replace(all_names, "sp\\. z o\\.o\\.", ""),
         all_names = str_replace(all_names, "s\\.r\\.o.", ""),
         all_names = str_replace(all_names, " a\\.s\\.", " as")) 

us_only <- chk %>% 
  filter(grepl("United States of America", country))


# will need editing: Access (One), Atari (Corporation), CAST (AI), Cision, Facebook, Sony 
# asian oceanian coputing industry organization



















