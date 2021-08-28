#' Match messy text and email data to social organizations
#'
#' Full explanation here... Combines text_to_org and email_to_org
#'
#' @param data A data frame.
#' @param id A numeric or character vector unique to each entry.
#' @param text Character vector of messy or unstructed text that will
#' be unnested as n-grams and matched to dictionary of academic instiutions.
#' @param output Desired name of organization column.
#' @param email Character vector of emails or email domains.
#'
#'#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified_users <- github_users %>%
#'   detect_countries(login, location, country_name, email)
#'

detect_countries <- function(data, id, text, output, email){ 
  # need to update email to default = FALSE
  
  print(paste("Started detect_countries() at", Sys.time()))
  
  # 1. convert all vars with enquos
  id <- enquo(id)
  text <- enquo(text)
  output <- enquo(output)
  email <- enquo(email)
  `%notin%` <- Negate(`%in%`)

  # 2. match by text entries 
  matched_by_text <- data %>%
    tidyorgs::text_to_countries(!!id, !!text, !!output)
  already_classified <- matched_by_text[,1]
  
  # 3. match by all emails 
  matched_by_email <- data %>%
    dplyr::filter(!!id %notin% already_classified) %>% # check
    tidyorgs::email_to_countries(!!id, !!email, !!output) 
  
  all_matched_data <- dplyr::bind_rows(matched_by_text, matched_by_email) 
  
  # todo: need to add an if clause in the case that email = FALSE
  data <- data %>% 
    dplyr::left_join(all_matched_data) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::group_by(!!id, !!text, !!email) %>%
    dplyr::mutate("{{output}}" := paste(!!output, collapse = "|")) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::mutate("{{output}}" := dplyr::na_if(!!output, "NA"))
  data
}






