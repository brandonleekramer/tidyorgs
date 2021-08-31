#' Convert messy text and email data into standardized country names or country codes
#'
#' This function standardizes messy text data that contains city, state, and/or country names 
#' as well as email domains into standardized country names. The detect_countries() function 
#' integrates tidyorgs's text_to_countries() and email_to_countries() together, providing 
#' the capacity to match users to countries for organizational or geographic analysis in a tidy framework.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param text Character vector of text data that includes the name of cities, states, 
#' and/or countries that will be standardized into country names or country codes.
#' @param output Country name or country code. Either "country_name", "iso2", or "iso3". 
#' If multiple countries are detected, they will be separated by the "|" symbol.
#' @param email Character vector of email or email domain information.
#'
#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified_users <- github_users %>%
#'   detect_countries(login, location, country_name, email)
#'   
#' @export
detect_countries <- function(data, id, text, output, email){ 
  # TODO: need to add an if clause in the case that email = FALSE
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
  # 4. join back to the original dataset and remove duplicates
  suppressMessages(
  data <- data %>% 
    dplyr::left_join(all_matched_data) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::group_by(!!id, !!text, !!email) %>%
    dplyr::mutate("{{output}}" := paste(!!output, collapse = "|")) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::mutate("{{output}}" := dplyr::na_if(!!output, "NA")) %>% 
    dplyr::ungroup())
  data
}