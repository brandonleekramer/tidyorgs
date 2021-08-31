#' Match emails to countries 
#'
#' This function matches email domains to countries based on email sub-domains. 
#' For example, users with an email domain ending in ".br" will be matched to Brazil. 
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of emails or email domains.
#' @param output Desired name of classified organization column.
#'
#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified_by_email <- github_users %>%
#'   email_to_countries(login, email, organization)
#'
#' @export
email_to_countries <- function(data, id, input, output){ 
  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  # 2. pull out all of the country domains from the dictionary 
  country_dictionary <- tidyorgs::countries_data
  country_dictionary <- country_dictionary %>%
    tidyr::drop_na(country_domain) %>%
    dplyr::select(country_domain, country_name) 
  country_vector <- na.omit(country_dictionary$country_domain)
  country_dictionary <- country_dictionary %>%
    dplyr::mutate(beginning = "\\b(?i)(", ending = ")\\b", 
                  country_domain = str_replace(country_domain, "\\.", ""),
                  country_domain = paste0(beginning, country_domain, ending)) %>%
    dplyr::select(country_domain, country_name) %>% tibble::deframe()
  # 3. drop missing emails, all domains to lower, extract domain info after @ sign
  all_domains_df <- data %>%
    tidyr::drop_na(!!input) %>% # drop missing emails
    dplyr::mutate("{{ input }}" := tolower(!!input)) %>% # all domains to lower
    dplyr::mutate(domain = sub('.*@', '', !!input)) %>% # extract domain info after @ sign
    dplyr::mutate(domain = sub('.*\\.', '.', domain)) %>% # extract the last .domain
    # matches all of the root domains with several exceptions (bc of industry appropriation)
    dplyr::filter(domain %in% country_vector & domain != ".me" & domain != ".cc" 
                  & domain != ".ai" & domain != ".fm" & domain != ".im" & domain != ".as") %>%  
    dplyr::mutate(domain = str_replace(domain, '\\.', '')) 
  #4. uses str_replace_all() to recode all domains into countries
  all_domains_df <- all_domains_df %>% 
    dplyr::mutate("{{output}}" := stringr::str_replace_all(domain, country_dictionary)) %>%
    dplyr::select(!!id, !!output)
  # 5. removes all of the duplicates and combines those with multiple countries
  all_domains_df <- all_domains_df %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::group_by(!!id, !!output) %>%
    dplyr::mutate("{{output}}" := paste(!!output, collapse = "|")) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::mutate("{{output}}" := dplyr::na_if(!!output, "NA"))
  all_domains_df
}