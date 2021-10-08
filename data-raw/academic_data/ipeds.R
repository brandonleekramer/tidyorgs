rm(list=ls())
library(tidyverse)

tidyorgs_academic <- read_csv("~/git/tidyorgs/data-raw/academic_institutions.csv")
ipeds_data <- read_csv("~/git/tidyorgs/data-raw/ipeds_summary_2019.csv")

# plan: link by email domain first, then move to names

tidy_usa <- tidyorgs_academic %>%
  filter(country == "United States") %>%
  mutate(to_names = new_string,
         linking_col = new_string,
         domains_original = domains) %>%
  select(academic_terms, original_string, to_names, domains, linking_col, domains_original) %>%
  separate_rows(domains, sep = "\\|")

ipeds_usa <- ipeds_data %>%
  rename(ipeds_unit_id = `Unit Id`,
         ipeds_names = `Institution Name`,
         ipeds_website = `Website address`,
         ope_id = `Office of Postsecondary Education (OPE) ID`) %>%
  select(starts_with("ipeds_"), ope_id)

ipeds_to_match <- ipeds_usa %>%
  mutate(domains = tolower(ipeds_website),
         domains = sub('https://', '', domains),
         domains = sub('www2.', '', domains),
         domains = sub('www.', '', domains),
         domains = sub('/$', '', domains),
         domains = sub('/.*', '', domains))


matched_domains <- ipeds_to_match %>%
  inner_join(tidy_usa, by = "domains") %>% 
  select(ipeds_unit_id, ipeds_names, to_names, ipeds_website, domains, 
         academic_terms, original_string, domains_original, ope_id)

dont_match_tidyorgs <- tidy_usa %>%
  anti_join(ipeds_to_match, by = "domains")

dont_match_ipeds <- ipeds_to_match %>%
  anti_join(tidy_usa, by = "domains")

matched_names <- dont_match_ipeds %>%
  mutate(linking_col = tolower(ipeds_names)) %>%
  inner_join(dont_match_tidyorgs, by = "linking_col") %>% 
  unite("domains", c("domains.x", "domains.y"), sep = "|", remove = TRUE) %>% 
  select(ipeds_unit_id, ipeds_names, to_names, ipeds_website, domains, 
         academic_terms, original_string, domains_original, ope_id) 

matched_both <- matched_domains %>% 
  bind_rows(matched_names) %>% 
  arrange(domains)
rm(matched_domains, dont_match_tidyorgs, dont_match_ipeds, matched_names)

matched_removed <- matched_both %>% 
  select(-domains) %>% 
  rename(domains = domains_original) %>%
  group_by(ipeds_unit_id, ipeds_names, ipeds_website, domains) %>% 
  mutate(to_names = paste0(to_names, collapse = "|")) %>%
  distinct(ipeds_unit_id, ipeds_names, to_names, ipeds_website, domains, 
           academic_terms, original_string, ope_id) %>% 
  ungroup() %>% 
  rename(new_string = ipeds_names) %>% 
  mutate(matched = 1) 

matched_ipeds_ids <- na.omit(matched_removed$ipeds_unit_id)
ipeds_to_join <- ipeds_usa %>% 
  filter(!ipeds_unit_id %in% matched_ipeds_ids) %>% 
  mutate(to_names = tolower(ipeds_names),
         to_names = sub('-', ' ', to_names),
         ipeds_website = tolower(ipeds_website),
         domains = ipeds_website,
         domains = sub('https://', '', domains),
         domains = sub('www2.', '', domains),
         domains = sub('www.', '', domains),
         domains = sub('/$', '', domains),
         domains = sub('/.*', '', domains)) %>% 
  mutate(academic_terms = to_names,
         original_string = to_names,
         matched = 0) %>% 
  rename(new_string = ipeds_names) %>% 
  select(academic_terms, original_string, new_string, domains, ipeds_unit_id, ope_id, matched)

ipeds_together <- matched_removed %>% 
  select(academic_terms, original_string, new_string, domains, ipeds_unit_id, ope_id, matched) %>% 
  bind_rows(ipeds_to_join) %>% 
  arrange(new_string) %>% 
  mutate(country = "United States",
         as_of = Sys.Date()) %>% 
  select(academic_terms, original_string, new_string, domains, 
         country, as_of, matched, ipeds_unit_id, ope_id) %>% 
  distinct(academic_terms, original_string, new_string, domains, 
           country, as_of, matched, ipeds_unit_id, ope_id)

## joinining in other ipeds data 

ipeds_hd2020 <- read_csv("~/git/tidyorgs/data-raw/ipeds_hd2020.csv")

ipeds_hd2020_select <- ipeds_hd2020 %>% 
  select(UNITID, INSTNM, IALIAS, ADDR, CITY, STABBR, 
         ZIP, COUNTYCD, LONGITUD, LATITUDE, WEBADDR, 
         EIN, DUNS, CONTROL, F1SYSTYP, F1SYSNAM) %>% 
  rename(ipeds_unit_id = UNITID, aliases = IALIAS, address = ADDR, city = CITY, us_state = STABBR, us_zip = ZIP,
         us_fips = COUNTYCD, longitude = LONGITUD, latitude = LATITUDE, website_url = WEBADDR, 
         ein_id = EIN, duns_id = DUNS, school_type = CONTROL, in_school_system = F1SYSTYP, school_system = F1SYSNAM) %>% 
  mutate(school_type = case_when(school_type == 1 ~ "Public",
                                 school_type == 2 ~ "Private not-for-profit",
                                 school_type == 3 ~ "Private for-profit"),
         school_type = na_if(school_type, "-3")) 
with_systems <- ipeds_hd2020_select %>% 
  drop_na(school_system)
without_systems <- ipeds_hd2020_select %>% 
  filter(is.na(school_system)) %>% 
  mutate(school_system = INSTNM) 
ipeds_hd2020_select <- with_systems %>% 
  bind_rows(without_systems) %>% 
  arrange(ipeds_unit_id) %>% 
  mutate(aliases = str_replace_all(aliases,"[^[:graph:]]", " "),
         aliases = str_replace_all(aliases, "\\|\\|", "|"),
         aliases = str_replace_all(aliases, " \\| ", "|"),
         aliases = str_replace_all(aliases, "-", " "),
         aliases = str_replace_all(aliases, ",", ""),
         aliases = tolower(aliases),
         aliases = str_replace(aliases, "  ", " ")) 

ipeds_with_features <- ipeds_together %>% 
  left_join(ipeds_hd2020_select, by = "ipeds_unit_id") %>% 
  select(-INSTNM)
  
# add in international data 
  
tidy_nonusa <- tidyorgs_academic %>%
  filter(country != "United States") %>%
  rename(ipeds_unit_id = ipeds_unitid) %>% 
  mutate(new_string = str_to_title(new_string),
         new_string = str_replace(new_string, " Of ", " of "),
         new_string = str_replace(new_string, " De ", " de "),
         new_string = str_replace(new_string, " Del ", " del "),
         new_string = str_replace(new_string, " Für ", " für "),
         new_string = str_replace(new_string, " And ", " and "),
         new_string = str_replace(new_string, " Et Les ", " et les ")) %>% 
  mutate(ope_id = NA, matched = NA, aliases = NA, address = NA, city = NA, us_state = NA, us_zip = NA, 
         us_fips = NA, longitude = NA, latitude = NA, website_url = NA, ein_id = NA, duns_id = NA, 
         school_type = NA, in_school_system = NA, school_system = NA) %>% 
  select(-status)

ipeds_plus_international <- ipeds_with_features %>% 
  bind_rows(tidy_nonusa) %>% 
  mutate(country = str_replace(country, "Bolivia, Plurinational State of", "Bolivia"),
         country = str_replace(country, "Brunei Darussalam", "Brunei"),
         country = str_replace(country, "Congo, the Democratic Republic of the", 
                               "Democratic Republic of the Congo"),
         country = str_replace(country, "Côte d'Ivoire", "Ivory Coast"),
         country = str_replace(country, "Holy See \\(Vatican City State\\)", "Vatican"),
         country = str_replace(country, "Korea, Democratic People's Republic of", "North Korea"),
         country = str_replace(country, "Korea, Republic of", "South Korea"),
         country = str_replace(country, "Lao People's Democratic Republic", "Laos"),
         country = str_replace(country, "Macedonia, the Former Yugoslav Republic of", "Macedonia"),
         country = str_replace(country, "Moldova, Republic of", "Moldova"),
         country = str_replace(country, "Palestine, State of", "Palestine"),
         country = str_replace(country, "Syrian Arab Republic", "Syria"),
         country = str_replace(country, "Tanzania, United Republic of", "Tanzania"),
         country = str_replace(country, "Venezuela, Bolivarian Republic of", "Venezuela"),
         country = str_replace(country, "Virgin Islands, British", "British Virgin Islands")) %>% 
  rename(catch_terms = academic_terms, recode_column = original_string, organization_name = new_string) %>% 
  select(catch_terms, recode_column, aliases, organization_name, 
         domains, website_url, school_type, in_school_system, school_system,
         country, address, city, us_state, us_zip, us_fips, longitude, latitude, 
         ends_with("_id"), matched, as_of) %>% 
  unite("all_names", c("recode_column", "aliases"), sep = "|", remove = FALSE) %>% 
  mutate(all_names = str_replace(all_names, "NA\\|NA", ""),
         all_names = str_replace(all_names, "NA\\|", ""),
         all_names = str_replace(all_names, "\\|NA", ""), 
         manually_edited = 0) 

dupe_domains <- ipeds_plus_international %>% 
  janitor::get_dupes(domains)
distinct_dupes <- dupe_domains %>% 
  distinct(domains) %>% 
  mutate(dupe_domain = 1)

ipeds_plus_international <- ipeds_plus_international %>% 
  left_join(distinct_dupes, by = "domains")

dupe_names <- ipeds_plus_international %>% 
  janitor::get_dupes(organization_name)
distinct_dupes <- dupe_names %>% 
  distinct(organization_name) %>% 
  mutate(dupe_name = 1)

ipeds_plus_international <- ipeds_plus_international %>% 
  left_join(distinct_dupes, by = "organization_name")

write_csv(ipeds_plus_international, "~/git/tidyorgs/data-raw/hipolabs_with_ipeds.csv")

## priorities 

# 1. duplicate domains 



################ all things yet to do 
# penn state system data seems wrong - add in a manually edited column 
# there are duplicates institutions - will manually curate 
# there are duplicates in ipeds numbers - why? 
# dhs.lacounty.gov ????
# typo on 118143









