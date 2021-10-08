rm(list=ls())
library(tidyverse)
library(janitor)

# cities 

us_gov_emails <- read_csv("~/git/tidyorgs/data-raw/government_data/08_usgov_federal_email_domains.csv") %>% 
  clean_names() %>% 
  mutate(domain_name = tolower(domain_name))

us_states <- read_csv("~/git/tidyorgs/data-raw/diverstidy - states.csv") %>% 
  select(state_abb1, state) %>% 
  rename(state_name = state, state = state_abb1)

us_gov_emails %>% 
  group_by(domain_type) %>%
  count()

city_govs <- us_gov_emails %>% 
  filter(domain_type == "City" 
         & grepl("City|CITY|Town|TOWN|town of|Village|VILLAGE", organization)) %>% 
  arrange(state) %>% 
  select(agency, domain_type, domain_name, organization, city, state) %>% 
  mutate(organization = gsub("(.*),.*", "\\1", organization),
         organization = str_to_title(organization),
         organization = str_replace(organization, " Of ", " of "),
         organization = str_replace(organization, " Police Department", ""),
         organization = str_replace(organization, " Parks & Recreation", ""),
         organization = str_replace(organization, " Publc Library", ""),
         organization = str_replace(organization, " Information Technology", ""),
         organization = str_replace(organization, " Emergency Communication District", "")) %>% 
  distinct_all() %>% 
  group_by(agency, domain_type, organization, city, state) %>% 
  mutate(domain_name = paste0(domain_name, collapse = "|")) %>% 
  ungroup() %>% 
  distinct_all() %>% 
  left_join(us_states, by = "state")

city_dupes <- city_govs %>% 
  get_dupes(organization) %>% 
  select(-dupe_count) %>% 
  mutate(original_org = organization,
         catch_with_state = organization, 
         organization = str_c(organization, ", ", state),
         catch_terms = tolower(organization),
         catch_terms = str_replace(catch_terms, ",", ""),
         catch_with_state = str_c(catch_with_state, " ", state_name),
         catch_with_state = tolower(catch_with_state),
         catch_terms = str_c(catch_terms, "(?![a-z])|", catch_with_state)) %>% 
  select(-catch_with_state)
city_dupes_orgs <- city_dupes %>% distinct(original_org)
city_dupes_orgs <- na.omit(city_dupes_orgs$original_org)

city_deduped <- city_govs %>% 
  filter(!organization %in% city_dupes_orgs) %>% 
  # adding state information and the catch terms 
  mutate(organization = str_c(organization, ", ", state),
         city = str_to_title(city),
         catch_terms = tolower(organization),
         catch_terms = gsub("(.*),.*", "\\1", catch_terms),
         catch_with_state = catch_terms,
         state_name = tolower(state_name),
         catch_with_abb = tolower(organization),
         catch_with_abb = str_replace(catch_with_abb, ",", ""),
         catch_terms = str_c(catch_terms, "(?! [a-z])|", catch_with_abb),
         catch_with_state = str_c(catch_with_state, " ", state_name),
         catch_terms = str_c(catch_terms, "(?![a-z])|", catch_with_state)) %>% 
  select(-catch_with_abb, -catch_with_state, -state_name) 

city_govs_deduped <- city_dupes %>% 
  select(catch_terms, organization, domain_name, domain_type, agency, city, state) %>% 
  bind_rows(
    city_deduped %>% 
      select(catch_terms, organization, domain_name, domain_type, agency, city, state)) %>%
  mutate(recode_column = catch_terms,
         catch_terms = str_replace(catch_terms, "\\(\\?\\!\\[a-z\\]\\)", ""),
         catch_terms = str_replace(catch_terms, "\\(\\?\\! \\[a-z\\]\\)", ""),
         organization = str_c(organization, " Government")) %>% 
  select(catch_terms, recode_column, everything()) %>% 
  mutate(domain_type = "City")

rm(city_govs, city_deduped, city_dupes)

# counties 

us_states <- read_csv("~/git/tidyorgs/data-raw/diverstidy - states.csv") %>% 
  select(state, state_abb1) %>% 
  rename(state_name = state, state = state_abb1) %>% 
  mutate(comma_name = str_c(", ", state_name), 
         comma_state = str_c(", ", state), 
         nothing = "")

commaname_to_nothing <- us_states %>% select(comma_name, nothing) %>% deframe()
name_to_nothing <- us_states %>% 
  mutate(state_name = str_c("\\b(",state_name,"$)\\b")) %>%
  select(state_name, nothing) %>% deframe()
comma_to_nothing <- us_states %>% select(comma_state, nothing) %>% deframe()
abb_to_nothing <- us_states %>% 
  mutate(state = str_c("\\b(",state,")\\b")) %>% 
  select(state, nothing) %>% deframe()
abb_to_state <- us_states %>% 
  select(state, state_name) %>% 
  mutate(state = str_c("\\b(",state,")\\b")) %>% deframe()

county_govs <- us_gov_emails %>% 
  filter(domain_type == "County" 
         | (domain_type == "State/Local Govt" & grepl("County|COUNTY", organization))
         & !(grepl("City|CITY|Town|TOWN|town of|Village|VILLAGE", organization))) %>% 
  mutate(city = str_to_title(city),
         organization = str_replace_all(organization, commaname_to_nothing),
         organization = str_replace_all(organization, name_to_nothing),
         organization = str_replace_all(organization, comma_to_nothing),
         organization = str_replace_all(organization, abb_to_nothing),
         organization = str_replace(organization, " EMS", ""),
         organization = str_to_title(organization),
         organization = str_replace(organization, " Of ", " of "),
         organization = str_replace(organization, " And ", " and "),
         organization = str_replace(organization, " The ", " the "),
         organization = str_replace(organization, "State of ", ""),
         # boards, commissioners, government, services 
         organization = str_replace(organization, " Board of Supervisors", ""),
         organization = str_replace(organization, " Board of County", ""),
         organization = str_replace(organization, " Board of Commissioners", ""),
         organization = str_replace(organization, " Booard of Commissioners", ""),
         organization = str_replace(organization, " Board of Elections", ""),
         organization = str_replace(organization, " Supervisor of Elections", ""),
         organization = str_replace(organization, "Supervisor of Elections ", ""),
         organization = str_replace(organization, " Commissioners Office", ""),
         organization = str_replace(organization, "Commissioners of ", ""),
         organization = str_replace(organization, " Commissioners", ""),
         organization = str_replace(organization, " Commission", ""),
         organization = str_replace(organization, " Local Government", ""),
         organization = str_replace(organization, " Units of Government", ""),
         organization = str_replace(organization, " Government", ""),
         organization = str_replace(organization, " Goverment", ""),
         organization = str_replace(organization, " Govt\\.", ""),
         organization = str_replace(organization, " Govt", ""),
         organization = str_replace(organization, "County Council of ", ""),
         organization = str_replace(organization, " Office of Public Affairs", ""),
         organization = str_replace(organization, " Mayor's Office", ""),
         organization = str_replace(organization, " Executive's Office", ""),
         organization = str_replace(organization, " Central Services", ""),
         organization = str_replace(organization, " Internal Services Department", ""),
         organization = str_replace(organization, "  Building and Planning Deptartment", ""),
         organization = str_replace(organization, "  Department of Finance", ""),
         # courts, corrections and emergency services 
         organization = str_replace(organization, " Sheriff Office", ""),
         organization = str_replace(organization, " Sheriff's Office", ""),
         organization = str_replace(organization, " Sheriff", ""),
         organization = str_replace(organization, " Fiscal Court", ""),
         organization = str_replace(organization, " Court House", ""),
         organization = str_replace(organization, " Judge Executive", ""),
         organization = str_replace(organization, " Probate Judge", ""),
         organization = str_replace(organization, " Circuit Courts", ""),
         organization = str_replace(organization, " Attorney", ""),
         organization = str_replace(organization, " Clerk of the Circuit Court and Comptroller", ""),
         organization = str_replace(organization, " Court House", ""),
         organization = str_replace(organization, " Police Jury", ""),
         organization = str_replace(organization, " Prosecutor's Office", ""),
         organization = str_replace(organization, " Circuit Clerk", ""),
         organization = str_replace(organization, " Central Dispatch", ""),
         organization = str_replace(organization, " Emergency Communications Center", ""),
         organization = str_replace(organization, " Fire Protection District", ""),
         organization = str_replace(organization, " Clerk's Office", ""),
         organization = str_replace(organization, " Tax Collector", ""),
         organization = str_replace(organization, " Detention Center", ""),
         organization = str_replace(organization, "-Coroner Department", ""),
         organization = str_replace(organization, " Emergency Management Agency", ""),
         organization = str_replace(organization, " Auditor", ""),
         organization = str_replace(organization, " Appraisal District", ""),
         organization = str_replace(organization, " 911", ""),
         # tech and communications 
         organization = str_replace(organization, " Information Technology", ""),
         organization = str_replace(organization, " Information Services", ""),
         organization = str_replace(organization, " Mis Department", ""),
         organization = str_replace(organization, " - Bureau of Technology", ""),
         organization = str_replace(organization, " Telecommunications", ""),
         organization = str_replace(organization, " I.t.", ""),
         organization = str_replace(organization, " Data Center", ""),
         organization = str_replace(organization, " Data Processing Dept.", ""),
         # odds and ends 
         organization = str_replace(organization, " Commissio", ""),
         organization = str_replace(organization, " Services", ""),
         organization = str_replace(organization, "'s Department", ""),
         organization = str_replace(organization, " Board of", ""),
         organization = str_replace(organization, " Board$", ""),
         organization = str_replace(organization, " Local$", ""),
         organization = str_replace(organization, " Elections", ""),
         organization = str_replace(organization, " Bcc", ""),
         organization = str_replace(organization, " Bocc", ""),
         organization = str_replace(organization, " Otc", ""),
         organization = str_replace(organization, " Dept\\.", ""),
         organization = str_replace(organization, " Courthouse", ""),
         organization = str_replace(organization, " Administrator", ""),
         organization = str_replace(organization, " Administration", ""),
         organization = str_replace(organization, ", ", ""),
         organization = trimws(organization)) %>% 
  left_join(us_states %>% select(state, state_name), by = "state") %>% 
  mutate(recode_column = str_c(tolower(organization)," ",tolower(state),"(?![a-z])"),
         recode_column = str_c(recode_column,"|",tolower(organization)," ",tolower(state_name)),
         catch_terms = recode_column,
         catch_terms = str_replace(catch_terms, "\\(\\?\\!\\[a-z\\]\\)", ""),
         organization = str_c(organization, ", ", state, " Government")) %>% 
  select(catch_terms, recode_column, organization, domain_name, domain_type, agency, city, state) %>% 
  distinct_all() %>% 
  group_by(catch_terms, recode_column, organization, domain_type, agency, city, state) %>% 
  mutate(domain_name = paste0(domain_name, collapse = "|")) %>% 
  ungroup() %>% 
  distinct_all()

cities_and_counties <- bind_rows(city_govs_deduped, county_govs) %>% 
  mutate(country = "United States")

# states 

state_govs <- us_gov_emails %>% 
  filter(domain_type == "State/Local Govt"
         & !(grepl("City|CITY|Town|TOWN|town of|Village|VILLAGE|County|COUNTY", organization))) %>% 
  arrange(state, city, organization) %>% 
  select(-security_contact_email) %>% 
  left_join(us_states %>% 
              select(state, state_name) %>% 
              add_row(state = "AS", state_name = "American Somoa") %>% 
              add_row(state = "GU", state_name = "Guam"), by = "state") %>% 
  mutate(city = str_to_title(city),
         organization = str_replace_all(organization, abb_to_state),
         organization = str_replace(organization, "\\b(AOC)\\b", "Alabama Office of Communications"),
         organization = str_replace(organization, "\\b(ADOT)\\b", "Arizona Department of Transportation"),
         organization = str_replace(organization, "\\b(AHCCCS)\\b", "Arizona Health Care Cost Containment System"),
         organization = str_replace(organization, "\\b(OCTFME)\\b", "DC Office of Cable Television, Film, Music and Entertainment"),
         organization = str_replace(organization, "\\b(DEO)\\b", "Department of Economic Opportunity"),
         organization = str_replace(organization, "\\b(OLITS)\\b", "Office of Legislative Information Technology Services"),
         organization = str_replace(organization, "\\b(OCIO)\\b", "Office of the Chief Information Officer"),
         organization = str_replace(organization, "\\b(CIO)\\b", "Office of the Chief Information Officer"),
         organization = str_replace(organization, "\\b(NJHMFA)\\b", "New Jersey Housing and Mortgage Finance Agency"),
         organization = str_replace(organization, "\\b(SCAQMD)\\b", "South Coast Air Quality Management District"),
         organization = str_replace(organization, "\\b(HHSDC)\\b", "Health and Human Services Data Center"),
         organization = str_replace(organization, "\\b(ITS|I.T.S)\\b", "Internal Tax Services"),
         organization = str_replace(organization, "\\b(DEP)\\b", "Department of Environmental Protection"),
         organization = str_replace(organization, "\\b(State of South Dakota BIT)\\b", "State of South Dakota Bureau of Information and Telecommunications"),
         organization = str_replace(organization, "\\b(ITD)\\b", "Information Technology Department"),
         organization = str_replace(organization, "\\b(DAGS/ICSD)\\b", "Department of Accounting and General Services"),
         organization = str_replace(organization, "\\b(Oregon State Data Center ETS)\\b", "Oregon State Data Center Emerging Technologies and Services"),
         organization = str_replace(organization, "\\b(Virginia ABC)\\b", "Virginia Alcoholic Beverage Control Authority"),
         organization = str_replace(organization, "\\b(DENR)\\b", "Department of Environment and Natural Resources"),
         organization = str_replace(organization, "\\b(CGIA)\\b", "Center for Geographic Information & Analysis"),
         organization = str_replace(organization, "\\b(, DISC)\\b", ""),
         organization = str_replace(organization, "N\\.C\\.", "North Carolina"),
         organization = str_replace(organization, "Governor's Office of Faith-Based and Community Initiatives", "Governor's Office of Faith-Based & Community Initiatives"),
         organization = str_replace(organization, "\\b(DIT)\\b", "Department of Information Technology"),
         organization = str_replace(organization, "\\b(LOSAP)\\b", "Length of Service Award Program"),
         organization = str_replace(organization, "\\b(CDC)\\b", "Centers for Disease Control"),
         organization = str_replace(organization, "\\b(South Carolina DHEC)\\b", "South Carolina Department of Health and Environmental Control"),
         organization = str_replace(organization, "Arizona ROC", "Arizona Registrar of Contractors"),
         organization = str_replace(organization, "Election Office", "American Somoa Election Office"),
         organization = str_replace(organization, "\\b(WVNET)\\b", "West Virginia Network for Educational Telecomputing"),
         organization = str_to_title(organization),
         organization = str_replace(organization, " Of ", " of "),
         organization = str_replace(organization, " And ", " and "),
         organization = str_replace(organization, " The ", " the "),
         organization = str_replace(organization, " On ", " on "),
         organization = str_replace(organization, " For ", " for "),
         organization = str_replace(organization, "It|I\\.t\\.", "IT"),
         state_organization = str_c("State Government of ", state_name),
         organization = str_replace(organization, " - ", " "),
         organization = str_replace(organization, " / ", " "),
         organization = str_replace(organization, ",", ""),
         organization = str_replace(organization, "Filelocal", "State of Washington FileLocal"),
         organization = str_replace(organization, "Mtelectionresults\\.gov", "mtelectionresults.gov"),
         organization = str_replace(organization, "Dept\\.", "Department"),
         organization = str_replace(organization, "Dept", "Department"),
         organization = str_replace(organization, "Departmentartment", "Department"),
         organization = str_replace(organization, "Dc", "Washington DC"),
         organization = str_replace(organization, "Department of Information System Arkansas", "Department of Information Systems Arkansas")) %>% 
  distinct_all() %>% 
  group_by(domain_type, agency, organization, city, state, state_name, state_organization) %>% 
  mutate(domain_name = paste0(domain_name, collapse = "|")) %>% 
  ungroup() %>% 
  distinct_all() %>% 
  select(-domain_name, domain_name) %>%
  mutate(domain_type = state_organization,
         recode_column = tolower(organization),
         recode_column = str_replace(recode_column, "&", "and"), 
         catch_terms = recode_column,
         country = "United States") %>% 
  select(catch_terms, recode_column, organization, domain_name, domain_type, agency, city, state, country) 

cities_counties_states <- bind_rows(cities_and_counties, state_govs)

# Independent Intrastate Agency    34
# Interstate Agency                17

intra_interstate <- us_gov_emails %>% 
  filter(domain_type == "Independent Intrastate Agency" 
         | domain_type == "Interstate Agency"
         | domain_type == "Interstate Agency"
         & !(grepl("City|CITY|Town|TOWN|town of|Village|VILLAGE|County|COUNTY", organization))) %>% 
  select(-security_contact_email) %>% 
  mutate(city = str_to_title(city),
         organization = str_replace(organization, "Appalachia HIDTA", "Appalachian High Intensity Drug Trafficking Area"),
         organization = str_replace(organization, "State of ND, ITD", "State of North Dakota, Information Technology Department"),
         organization = str_to_title(organization),
         organization = str_replace(organization, " Of ", " of "),
         organization = str_replace(organization, " And ", " and "),
         organization = str_replace(organization, " The ", " the "),
         organization = str_replace(organization, " On ", " on "),
         organization = str_replace(organization, " For ", " for "),
         organization = str_replace(organization, "Mn ", "Minnesota "),
         organization = str_replace(organization, "Mmcap Infuse", "MMCAP Infuse")) %>% 
  distinct_all() %>% 
  group_by(domain_type, agency, organization, city, state) %>% 
  mutate(domain_name = paste0(domain_name, collapse = "|")) %>% 
  ungroup() %>% 
  distinct_all() %>% 
  select(-domain_name, domain_name) %>%
  mutate(recode_column = tolower(organization),
         recode_column = str_replace(recode_column, "&", "and"), 
         catch_terms = recode_column,
         country = "United States") %>% 
  select(catch_terms, recode_column, organization, domain_name, domain_type, agency, city, state, country) 
  
cities_counties_states <- bind_rows(cities_counties_states, intra_interstate)

# Native Sovereign Nation         192

native_nations <- us_gov_emails %>% 
  filter(domain_type == "Native Sovereign Nation" 
         & !(grepl("City|CITY|Town|TOWN|town of|Village|VILLAGE|County|COUNTY", organization))) %>% 
  select(-security_contact_email) %>% 
  mutate(city = str_to_title(city),
         organization = str_replace_all(organization, "Muscogee\\(Creek\\)Nation", "Muscogee (Creek) Nation"),
         organization = str_to_title(organization),
         organization = str_replace_all(organization, " Of ", " of "),
         organization = str_replace_all(organization, " And ", " and "),
         organization = str_replace_all(organization, " The ", " the "),
         organization = str_replace_all(organization, " On ", " on "),
         organization = str_replace_all(organization, " In ", " in "),
         organization = str_replace_all(organization, " For ", " for "),
         organization = str_replace_all(organization, "Coeur D' Alene", "Coeur d'Alene"),
         
         ) %>% 
  distinct_all() %>% 
  group_by(domain_type, agency, organization, city, state) %>% 
  mutate(domain_name = paste0(domain_name, collapse = "|")) %>% 
  ungroup() %>% 
  distinct_all() %>% 
  select(-domain_name, domain_name) %>%
  mutate(recode_column = tolower(organization),
         recode_column = str_replace(recode_column, "&", "and"), 
         recode_column = str_replace(recode_column, "-", " "), 
         catch_terms = recode_column,
         country = "United States") %>% 
  select(catch_terms, recode_column, organization, domain_name, 
         domain_type, agency, city, state, country)

cities_counties_states <- bind_rows(cities_counties_states, native_nations)

# federal agencies 

federal_agencies <- us_gov_emails %>% 
  filter(grepl("Federal Agency", domain_type)
         & !(grepl("City|CITY|Town|TOWN|town of|Village|VILLAGE|County|COUNTY", organization))) %>% 
  select(-security_contact_email) %>% 
  distinct_all() %>% 
  group_by(domain_type, agency, organization, city, state) %>% 
  mutate(domain_name = paste0(domain_name, collapse = "|")) %>% 
  ungroup() %>% 
  distinct_all() %>% 
  mutate(recode_column = tolower(organization),
         catch_terms = tolower(organization),
         country = "United States") %>% 
  select(catch_terms, recode_column, organization, domain_name, 
         domain_type, agency, city, state, country)

write_csv(federal_agencies, "~/git/tidyorgs/data-raw/government_data/13_dotgov_federal.csv")






