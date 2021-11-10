
rm(list = ls())
library(tidyverse)
library(janitor)
library(purrr)

setwd("/project/biocomplexity/sdad/projects_data/ncses/oss/companies_scraped/aggregate")
all_businesses <- read_csv("sec_all_binded.csv")

business_types <- all_businesses %>% 
  rename(cik_code = cik) %>% 
  distinct(cik_code, company, state, country) %>% 
  mutate(original = company, state = toupper(state), company = toupper(company),
         company = str_replace_all(company, "SIC", " SIC"),
         sic_code = sub(".*SIC: ", "", company),
         sic_code = ifelse(str_detect(sic_code, pattern = "^[0-9]* -"), yes = sic_code, no = "NO SIC"),
         sic_code = na_if(sic_code, "NO SIC"),
         company = sub("SIC:.*", "", company),
         company = sub(' /DE/|/WY|/MS|/CO/|/NV/|/WA/|/NEW/|/OHIO/|/AL/|/AL|/TA|/MSD|/FA/|/CT|/GFN|/PA/|/ADR/|/ADR', '', company), # /MSD??
         company = trimws(company),
         business_type = company, 
         business_type = ifelse(str_detect(business_type, "\\b(?i)(LLC|LLLC|LLC|L L C|LLC$|L\\.L\\.C\\.$|L\\.L\\.C$)\\b"), "LLC", no = business_type),
         #business_type = ifelse(str_detect(business_type, "LLC$|L\\.L\\.C\\.$|L\\.L\\.C$"), "LLC", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(LP|LLP|LLLP|L P|L.P.|PARTNERSHIP|Limited Partnership|LLP$|LP$|L\\.P)\\b"), "PARTNERSHIP", no = business_type),
         #business_type = ifelse(str_detect(business_type, "L\\.\\P.$|Limited Partnership|LLP$|LP$|L\\.P"), "PARTNERSHIP", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(TRUST|REIT)\\b"), "TRUST", no = business_type),
         business_type = ifelse(str_detect(business_type, 
           "\\b(?i)(CORP|CORP|INC|INC|INCORPORATED|BANCORP|BANCORPORATION|LTD|LIMITED| CO |CO.|CO.$|CO$|PLC| CO\\.)\\b"), 
           "CORPORATION", no = business_type),
         #business_type = ifelse(str_detect(business_type, "/MSD|LIMITED.$| LTD|LTD$|LTD.$"), "CORPORATION", no = business_type),
         business_type = ifelse(str_detect(business_type, pattern = "CORPORATION|LLC|PARTNERSHIP|TRUST"), business_type, "NO CODE"), 
         business_type = na_if(business_type, "NO CODE")) 

non_yet_coded <- business_types %>% 
  filter(!(business_type %in% c("CORPORATION", "LLC", "PARTNERSHIP", "TRUST", "JOINT VENTURE", "NONPROFIT"))) %>%
  select(cik, company, sic_code, business_type, state, country, original)

already_coded <- business_types %>% 
  filter(business_type %in% c("CORPORATION", "LLC", "PARTNERSHIP", "TRUST", "JOINT VENTURE", "NONPROFIT")) %>% 
  drop_na(business_type) 
business_types <- business_types %>% 
  filter(cik %notin% already_coded$cik) %>% 
  mutate(business_type = ifelse(str_detect(company, "NON-PROFIT|FOUNDATION"), "NONPROFIT", no = business_type),
         business_type = ifelse(str_detect(company, "JOINT VENTURE"), "JOINT VENTURE", no = business_type)) %>%
  filter(business_type %in% c("NONPROFIT", "JOINT VENTURE")) %>% 
  bind_rows(business_types)
already_coded <- business_types %>% 
  filter(business_type %in% c("CORPORATION", "LLC", "PARTNERSHIP", "TRUST", "JOINT VENTURE", "NONPROFIT")) %>% 
  drop_na(business_type) 
still_to_code <- business_types %>% 
  filter(cik %notin% already_coded$cik)



words <- still_to_code %>% 
  tidytext::unnest_tokens(words, company) %>% 
  count(words) %>% 
  arrange(-n)

## 

counts_by_state <- business_types %>% 
  group_by(state) %>% 
  count()

business_types %>% 
  filter(grepl("&", sic_code) & grepl(" CO\\.$", company)) 


setwd("/project/biocomplexity/sdad/projects_data/ncses/oss/companies_scraped/aggregate")
write_csv(business_types, "business_types.csv")


business_types_small <- business_types %>% 
  mutate(company = tolower(company), 
         company = gsub("/.*", "\\1", company), 
         company = str_replace_all(company, ",", ""),
         company = str_replace_all(company, "-", ""),
         company = str_replace_all(company, "  ", " ")
         ) 


setwd("/project/biocomplexity/sdad/projects_data/ncses/oss/companies_scraped/aggregate")
write_csv(business_types, "business_types.csv")









  
