
rm(list = ls())
library(tidyverse)
library(janitor)
library(purrr)

# had to copy the folder and remove the empty dfs (AS, MP, UM)
setwd("/project/biocomplexity/sdad/projects_data/ncses/oss/companies_scraped/")
all_state_businesses <- list.files(pattern="*.csv$") %>% 
  map_df(~read_csv(.)) %>% 
  clean_names() %>% 
  #rename(state = state_country) %>% 
  mutate(country = "UNITED STATES") %>% 
  select(-x1)
#setwd("~/git/dolthub-challenge/data/")
#write_csv(all_state_businesses, "combined_raw_data.csv")

setwd("/project/biocomplexity/sdad/projects_data/ncses/oss/companies_scraped/aggregate")
binded_states <- read_csv("combined_raw_data.csv") %>% 
  mutate(country = str_replace(country, "UNITED STATES", "United States")) %>% 
  select(cik, company, country, state)

# had to copy the folder and remove the empty dfs (AS, MP, UM)
setwd("/project/biocomplexity/sdad/projects_data/ncses/oss/companies_scraped/countries")
Georgia <- read_csv('GEORGIA.csv') %>% mutate(country = "Georgia") %>% select(-`State/Country`)
anguilla <- read_csv("ANGUILLA.csv") %>% mutate(country = "Anguilla")%>% select(-`State/Country`)
chile <- read_csv('CHILE.csv') %>% mutate(country = "Chile")%>% select(-`State/Country`)
china <- read_csv('CHINA.csv') %>% mutate(country = "China")%>% select(-`State/Country`)
taiwan <- read_csv('TAIWAN.csv') %>% mutate(country = "Taiwan")%>% select(-`State/Country`)
cyprus <- read_csv('CYPRUS.csv') %>% mutate(country = "Cyprus")%>% select(-`State/Country`)
denmark <- read_csv('DENMARK.csv') %>% mutate(country = "Denmark")%>% select(-`State/Country`)
colombia <- read_csv('Colombia.csv') %>% mutate(country = "Colombia")%>% select(-`State/Country`)
comoros <- read_csv('Comoros.csv') %>% mutate(country = "Comoros")%>% select(-`State/Country`)
moldova <- read_csv('MOLDOVA.csv') %>% mutate(country = "Moldova")%>% select(-`State/Country`)
latvia <- read_csv('LATVIA.csv') %>% mutate(country = "Latvia")%>% select(-`State/Country`)
costa_rica <- read_csv('COSTA RICA.csv') %>% mutate(country = "Costa Rica")%>% select(-`State/Country`)
cook_islands <- read_csv('COOK ISLANDS.csv') %>% mutate(country = "Cook Islands")%>% select(-`State/Country`)
Dominican_Republic <- read_csv('DOMINICAN REPUBLIC.csv') %>% mutate(country = "Dominican Republic")%>% select(-`State/Country`)
Ecuador <- read_csv('ECUADOR.csv') %>% mutate(country = "Ecuador")%>% select(-`State/Country`)
Egypt <- read_csv('EGYPT.csv') %>% mutate(country = "Egypt")%>% select(-`State/Country`)
Ethiopia <- read_csv('ETHIOPIA.csv') %>% mutate(country = "Ethiopia")%>% select(-`State/Country`)
France <- read_csv('France.csv') %>% mutate(country = "France")%>% select(-`State/Country`)
Finland <- read_csv('Finland.csv') %>% mutate(country = "Finland")%>% select(-`State/Country`)
Ghana <- read_csv('Ghana.csv') %>% mutate(country = "Ghana")%>% select(-`State/Country`)
Slovakia <- read_csv('SLOVAKIA.csv') %>% mutate(country = "Slovakia")%>% select(-`State/Country`)
Russian_Federation <- read_csv('Russia.csv') %>% mutate(country = "Russian Federation")%>% select(-`State/Country`)
Gilbraltar <- read_csv('Gilbraltar.csv') %>% mutate(country = "Gilbraltar")%>% select(-`State/Country`)
Ukraine <- read_csv('Ukraine.csv') %>% mutate(country = "Ukraine")%>% select(-`State/Country`)
Botswana <- read_csv('Botswana.csv') %>% mutate(country = "Botswana")%>% select(-`State/Country`)
Belize <- read_csv('Belize.csv') %>% mutate(country = "Belize")%>% select(-`State/Country`)
Estonia <- read_csv('Estonia.csv') %>% mutate(country = "Estonia")%>% select(-`State/Country`)
Bolivia <- read_csv('Bolivia.csv') %>% mutate(country = "Bolivia")%>% select(-`State/Country`)
Brazil <- read_csv('Brazil.csv') %>% mutate(country = "Brazil")%>% select(-`State/Country`)
US_Outlying <- read_csv('US Outlying.csv') %>% mutate(country = "United States Minor Outlying Islands")%>% select(-`State/Country`)
Palestine <- read_csv('Palestine.csv') %>% mutate(country = "State of Palestine")%>% select(-`State/Country`)
French_Southern_Territories <- read_csv('French Southern Territories.csv') %>% 
  mutate(country = "French Southern Territories")%>% select(-`State/Country`)
Czech_Republic <- read_csv('Czech Republic.csv') %>% mutate(country = "Czech Republic")%>% select(-`State/Country`)
Germany <- read_csv('Germany.csv') %>% mutate(country = "Germany")%>% select(-`State/Country`)
Armenia <- read_csv('Armenia.csv') %>% mutate(country = "Armenia")%>% select(-`State/Country`)
Kazakistan <- read_csv('Kazakistan.csv') %>% mutate(country = "Kazakistan")%>% select(-`State/Country`)
Marshall_Islands <- read_csv('Marshall Islands.csv') %>% mutate(country = "Marshall Islands")%>% select(-`State/Country`)
Antigua_and_Barbuda <- read_csv('Antigua and Barbuda.csv') %>% mutate(country = "Antigua and Barbuda")%>% select(-`State/Country`)
United_Arab_Emirates <- read_csv('United Arab Emirates.csv') %>% mutate(country = "United Arab Emirates")%>% select(-`State/Country`)
Argentina <- read_csv('Argentina.csv') %>% mutate(country = "Argentina")%>% select(-`State/Country`)
Australia <- read_csv('Australia.csv') %>% mutate(country = "Australia")%>% select(-`State/Country`)
Austria <- read_csv('Austria.csv') %>% mutate(country = "Austria")%>% select(-`State/Country`)
Bahamas <- read_csv('Bahamas.csv') %>% mutate(country = "Bahamas")%>% select(-`State/Country`)
Bahrain <- read_csv('Bahrain.csv') %>% mutate(country = "Bahrain")%>% select(-`State/Country`)
Barbados <- read_csv('Barbados.csv') %>% mutate(country = "Barbados")%>% select(-`State/Country`)
Bermuda <- read_csv('Bermuda.csv') %>% mutate(country = "Bermuda")%>% select(-`State/Country`)
Bulgaria <- read_csv('Bulgaria.csv') %>% mutate(country = "Bulgaria")%>% select(-`State/Country`)
Cayman_Islands <- read_csv('Cayman Islands.csv') %>% mutate(country = "Cayman Islands")%>% select(-`State/Country`)
Cape_Verde <- read_csv('Cape Verde.csv') %>% mutate(country = "Cape Verde")%>% select(-`State/Country`)
British_Virgin_Islands <- read_csv('British Virgin Islands.csv') %>% 
  mutate(country = "British Virgin Islands")%>% select(-`State/Country`)
Brunei <- read_csv('Brunei.csv') %>% mutate(country = "Brunei")%>% select(-`State/Country`)
British_Indian_Ocean_Territory <- read_csv('British Indian Ocean Territory.csv') %>% 
  mutate(country = "British Indian Ocean Territory")%>% select(-`State/Country`)
Belguim <- read_csv('BELGIUM.csv') %>% mutate(country = "Belguim") %>% 
  select(CIK, Company, country)

binded_countries <- bind_rows(British_Indian_Ocean_Territory, Brunei, British_Virgin_Islands, Cape_Verde, 
          Bulgaria, Bermuda, Barbados, Bahrain, Austria, Australia, Argentina, United_Arab_Emirates, 
          Antigua_and_Barbuda, Marshall_Islands, Kazakistan, Armenia, Germany, Czech_Republic,
          French_Southern_Territories, Palestine, US_Outlying, Brazil, Bolivia, Estonia, Belize,  
          Ukraine, Gilbraltar, Russian_Federation, Slovakia, Ghana, Finland, France, Ethiopia, Egypt, 
          Ecuador, Dominican_Republic, cook_islands, costa_rica, latvia, moldova, comoros, colombia, 
          denmark, cyprus, taiwan, china, chile, anguilla, Georgia, Cayman_Islands, Botswana, Belguim) %>% 
  select(-`...1`) %>% mutate(state = as.character(NA)) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(cik) | !is.na(company))

Alberta <- read_csv('Alberta.csv') %>% 
  mutate(country = "Canada", state = "Alberta")
British_Columbia <- read_csv('British Columbia.csv') %>% 
  mutate(country = "Canada", state = "British Columbia")
Manitoba <- read_csv('Manitoba.csv') %>% 
  mutate(country = "Canada", state = "Manitoba")
New_Brunswick <- read_csv('New Brunswick.csv') %>% 
  mutate(country = "Canada", state = "New Brunswick")
Newfoundland <- read_csv('Newfoundland.csv') %>% 
  mutate(country = "Canada", state = "Newfoundland")
Nova_Scotia <- read_csv('Nova Scotia.csv') %>% 
  mutate(country = "Canada", state = "Nova Scotia")
Ontario <- read_csv('Ontario.csv') %>% 
  mutate(country = "Canada", state = "Ontario")
Yukon <- read_csv('Yukon.csv') %>% 
  mutate(country = "Canada", state = "Yukon")
Quebec <- read_csv('Quebec.csv') %>% 
  mutate(country = "Canada", state = "Quebec")
Saskatchewan <- read_csv('Saskatchewan.csv') %>% 
  mutate(country = "Canada", state = "Saskatchewan")
Prince_Edward_Island <- read_csv('Prince Edward Island.csv') %>% 
  mutate(country = "Canada", state = "Prince Edward Island")

binded_canada <- bind_rows(Alberta, British_Columbia, Manitoba, New_Brunswick, Saskatchewan, 
                           Newfoundland, Nova_Scotia, Ontario, Yukon, Quebec, Prince_Edward_Island) %>% 
  clean_names() %>% select(cik, company, country, state) %>% 
  filter(!is.na(cik) | !is.na(company))

binded_all <- bind_rows(binded_canada, binded_countries, binded_states)

setwd("~/git/tidyorgs/data-raw/business_data/02_sec_data/")
write_csv(binded_all, "sec_all_binded.csv")

setwd("/project/biocomplexity/sdad/projects_data/ncses/oss/companies_scraped/aggregate")
write_csv(binded_all, "sec_all_binded.csv")







#cen_af_rep <- read_csv("CENTRAL AFRICAN REPUBLIC.csv") # empty
#micronesia <- read_csv("MICRONESIA.csv") # weirdness - no businesses
# sri_lanka <- read_csv("SRI LANKA.csv") # empty
# chad <- read_csv('CHAD.csv')
# christmas_island <- read_csv('CHRISTMAS ISLAND.csv')
# benin <- read_csv('BENIN.csv')
# cocos_islands <- read_csv('Cocos Islands.csv')
# lithuania <- read_csv('LITHUANIA.csv') %>% mutate(country = "Lithuania")
# south_ga <- read_csv('SOUTH GEORGIA.csv') %>% mutate(country = "South Georgia")
# congo <- read_csv('CONGO.csv') %>% mutate(country = "Congo")
# Slovenia <- read_csv('SLOVENIA.csv') %>% mutate(country = "Slovenia")
# Cuba <- read_csv('CUBA.csv') %>% mutate(country = "Cuba")
# Dominica <- read_csv('DOMINICA.csv') %>% mutate(country = "Dominica")
# El_Salvador <- read_csv('EL SALVADOR.csv') %>% mutate(country = "El Salvador")
# Kyrgyzstan <- read_csv('KYRGYZSTAN.csv') %>% mutate(country = "Kyrgyzstan")
# Gabon <- read_csv('Gabon.csv') %>% mutate(country = "Gabon")
# Bosnia_and_Herzegovina <- read_csv('BOSNIA AND HERZEGOVINA.csv') %>% mutate(country = "Bosnia and Herzegovina.csv")
# Algeria <- read_csv('Algeria.csv') %>% mutate(country = "Algeria")
# Fiji <- read_csv('Fiji.csv') %>% mutate(country = "Fiji")
# Falkland_Islands <- read_csv('Falkland Islands.csv') %>% mutate(country = "Falkland Islands")
# Palau <- read_csv('Palau.csv') %>% mutate(country = "Palau")
# Turkmenistan <- read_csv('Turkmenistan.csv') %>% mutate(country = "Turkmenistan")
# Albania <- read_csv('Albania.csv') %>% mutate(country = "Albania")
# Belarus <- read_csv('Belarus.csv') %>% mutate(country = "Belarus")
# Afghanistan <- read_csv('Afghanistan.csv') %>% mutate(country = "Afghanistan")
# American_Samoa <- read_csv('American Samoa.csv') %>% mutate(country = "American Samoa")
# Angola <- read_csv('Angola.csv') %>% mutate(country = "Angola")
# Bhutan <- read_csv('Bhutan.csv') %>% mutate(country = "Bhutan")
# Eritrea <- read_csv('Eritrea.csv') %>% mutate(country = "Eritrea")
# Equitorial_Guinea <- read_csv('Equitorial Guinea.csv') %>% mutate(country = "Equitorial Guinea")
# Faroe_Islands <- read_csv('Faroe Islands.csv') %>% mutate(country = "Faroe Islands")
# Tuvala <- read_csv('Tuvala.csv') %>% mutate(country = "Tuvala")
# Tajikistan <- read_csv('Tajikistan.csv') %>% mutate(country = "Tajikistan")
# Mayotte <- read_csv('Mayotte.csv') %>% mutate(country = "Mayotte.csv")
# French_Guiana <- read_csv('French Guiana.csv') %>% mutate(country = "French Guiana")
# French_Polynesia <- read_csv('French Polynesia.csv') %>% mutate(country = "French Polynesia")
# Macedonia <- read_csv('Macedonia.csv') %>% mutate(country = "Macedonia")
# Kiribati <- read_csv('Kiribati.csv') %>% mutate(country = "Kiribati")
# New_Calcedonia <- read_csv('New Calcedonia.csv') %>% mutate(country = "New Calcedonia")
# Uzbekistan <- read_csv('Uzbekistan.csv') %>% mutate(country = "Uzbekistan")
# Antartica <- read_csv('Antartica.csv') %>% mutate(country = "Antartica")
# Djibouti <- read_csv('Djibouti.csv') %>% mutate(country = "Djibouti")
# Cameroon <- read_csv('Cameroon.csv') %>% mutate(country = "Cameroon")
# Burundi <- read_csv('Burundi.csv') %>% mutate(country = "Burundi")
# Myanmar <- read_csv('Myanmar.csv') %>% mutate(country = "Myanmar")
# Solomon_Islands <- read_csv('Solomon Islands.csv') %>% mutate(country = "Solomon Islands")
# Bouvet_Island <- read_csv('Bouvet Island.csv') %>% mutate(country = "Bouvet Island")
# Andorra <- read_csv('Andorra.csv') %>% mutate(country = "Andorra")
# Vanuatan <- read_csv('Vanuatan.csv') %>% mutate(country = "Vanuatan")
# Northern_Mariana_Islands <- read_csv('Northern Mariana Islands.csv') %>% mutate(country = "Northern Mariana Islands")
# Gambia <- read_csv('Gambia.csv') %>% mutate(country = "Gambia")
# East_Timor <- read_csv('East Timor.csv') %>% mutate(country = "East Timor")
# croatia <- read_csv('CROATIA.csv') %>% mutate(country = "Croatia")
# Cambodia <- read_csv('Cambodia.csv') %>% mutate(country = "Cambodia")
# christmas_island <- read_csv('CHRISTMAS ISLAND.csv') %>% mutate(country = "Christmas Island")
# Aruba <- read_csv('Aruba.csv') %>% mutate(country = "Aruba")
# Azerbaijan <- read_csv('AZERBAIJAN.csv') %>% mutate(country = "Azerbaijan")
# Bangladesh <- read_csv('Bangladesh.csv') %>% mutate(country = "Bangladesh")

