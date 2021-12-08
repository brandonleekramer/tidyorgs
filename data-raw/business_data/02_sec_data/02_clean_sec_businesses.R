
rm(list = ls())
library(tidyverse)
library(janitor)
library(purrr)

setwd("/project/biocomplexity/sdad/projects_data/ncses/oss/companies_scraped/aggregate")
all_businesses <- read_csv("sec_all_scraped_businesses.csv")

clean_weird_stuffs = '/DE/|/NEW|/WY|/MS|/CO/|/NV/|/WA/|/NEW/|/OHIO/|/AL/|/AL|/TA|/MSD|/FA/|/CT|/GFN|/PA/|/ADR/|/ADR|/FI/|/FI|//FI|/CN|/CANADA|/CAN|/ FEDERAL|/NY|/ADV|/NJ|/BD|/DE|/TA|/NV|/OR/|/GA/'

step_1 <- all_businesses %>% 
  rename(cik_code = cik) %>% 
  distinct(cik_code, company, state, country) %>% 
  mutate(original = company, 
         state = if_else(country != "Canada", toupper(state), state), 
         # prepping for sic code 
         company = toupper(company),
         company = str_replace_all(company, "SIC", " SIC"),
         sic_code = sub(".*SIC: ", "", company),
         sic_code = ifelse(str_detect(sic_code, pattern = "^[0-9]* -"), yes = sic_code, no = "NO SIC"),
         sic_code = na_if(sic_code, "NO SIC"),
         sic_code = str_to_title(sic_code),
         company = sub("SIC:.*", "", company),
         # prepping column for catch_terms 
         company = str_replace_all(company, clean_weird_stuffs, ''), 
         company = trimws(company),
         company = sub("LTD\\.", "LTD", company),
         company = sub("INC\\.", "INC", company),
         company = sub(", INC", " INC", company),
         company = sub("CORP\\.", "CORP", company),
         company = sub("CO\\.", "CO", company),
         company = sub("L\\.L\\.C\\.", "LLC", company),
         company = sub("L\\.L\\.C", "LLC", company),
         company = sub("L L C", "LLC", company),
         company = sub(", LP", " LP", company),
         company = sub(", L P", " LP", company),
         company = sub(", L P", " LP", company),
         company = sub(" L P", " LP", company),
         company = sub("L\\.P\\.", "LP", company))
         
# prepping for business types          
step_2 <- step_1 %>% 
  mutate(business_type = company, 
         business_type = ifelse(str_detect(business_type, "\\b(?i)(LLC)\\b"), "LLC", no = business_type),
         business_type = ifelse(str_detect(business_type, " A\\.S\\.| S\\.R\\.L\\."), "LLC", no = business_type), 
         business_type = ifelse(str_detect(business_type, " S A|S\\.A\\.|S\\.P\\.A\\.|NYRT\\.|N\\.V\\.| AG| SA| A/S| A S| NV| PLC"), "Limited Liability Public Company", no = business_type),
         business_type = ifelse(str_detect(business_type, " GMBH| BC| LDA|SP Z O\\.O\\.| OOO| TOB|S\\.R\\.O\\.|S\\.A\\.R\\.L\\.| SARL|S\\.R\\.L\\.|KFT\\.| B\\.V\\."), "Private Limited Company", no = business_type), 
         business_type = ifelse(str_detect(business_type, "\\b(?i)(LP|LLP|LLLP|PARTNERSHIP|LIMITED PARTNERSHIP)\\b"), "Limited Liability Partnership", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(TRUST|REIT)\\b"), "Trust", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(ULC)\\b"), "Unlimited Liability Company", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(JOINT VENTURE)\\b"), "Joint Venture", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(COOPERATIVE)\\b"), "Cooperative", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(INCOME FUND|VENTURE FUND|VENTURE CAPITAL|FUND|FUNDS|CAPITAL GROUP|CAPITAL|FINANCE|FINANCIAL|BANK|INVESTMENTS|INVESTORS|FINANCING|BOND|BONDS|SECURITIES|ASSET BACKED|CERTIFICATES|MORTGAGE|HOLDING|HOLDINGS)\\b"), "Banking/Finance", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)( OIL| DRILLING|PETRO|MINING|MINES|ENERGY|METALS|MINERALS)\\b"), "Oil/Mining/Energy", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(ASSOCIATES|PARTNERS)\\b"), "Law/Accounting", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(NONPROFIT|NON-PROFIT|FOUNDATION)\\b"), "Non-Profit", no = business_type),
         business_type = ifelse(str_detect(business_type, "\\b(?i)(CORPORATION|CORP|CORP|INC|INC|INCORPORATED|BANCORP|BANCORPORATION|LTD|LIMITED| CO |CO.|CO.$|CO$|PLC| CO\\.| CO)\\b"), 
                                "Corporation", no = business_type)) 

not_classified <- step_2 %>% 
  filter(!(business_type %in% c("Corporation", "LLC", "Partnership", "Trust", "Joint Venture", "Limited Liability Public Company", "Banking/Finance", "Non-Profit", 
                                "Law/Accounting", "Oil/Mining/Energy", "Joint-Stock Company", "Cooperative", "Unlimited Liability Company")))

step_2 <- step_2 %>% 
  filter(business_type %in% c("Corporation", "LLC", "Partnership", "Trust", "Joint Venture", "Limited Liability Public Company", "Banking/Finance", "Non-Profit", 
                                "Law/Accounting", "Oil/Mining/Energy", "Joint-Stock Company", "Cooperative", "Unlimited Liability Company"))


step_3 <- step_2 %>% 
  mutate(catch_terms = tolower(company), 
         catch_terms = sub(" ltd", "", catch_terms),
         catch_terms = sub(" inc", "", catch_terms),
         catch_terms = sub(" corp", "", catch_terms),
         catch_terms = sub(" lp", "", catch_terms),
         recode_column = catch_terms, 
         organization_name = str_to_title(company),
         organization_name = sub("Ltd", "Ltd.", organization_name),
         organization_name = sub("Inc", "Inc.", organization_name),
         organization_name = sub("Corp", "Corp.", organization_name),
         organization_name = sub("Lp", "L.P.", organization_name),
         organization_name = sub(" Ulc", " ULC", organization_name),
         organization_name = sub(" Iii", " III", organization_name),
         organization_name = sub(" Llc", " LLC", organization_name),) %>% 
  select(catch_terms, recode_column, organization_name, state, country, cik_code, sic_code, original)








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









  
