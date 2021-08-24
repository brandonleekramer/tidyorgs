
library(tidyverse)
setwd("data-raw/government_data/12_kosack_state_gov_data")
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)
state_data <- vroom(list_of_files, id = "file_name")

state_data <- state_data %>% 
  mutate(state = str_replace(file_name, "./", ""),
         state = str_replace(state, "\\_nodes.txt", ""),
         state = toupper(state)) %>% 
  select(state, NodeName, NodeURL)

write_csv(state_data, "02_state_data_raw.csv")


