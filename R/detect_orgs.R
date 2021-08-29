#' Match messy text and email data to academic, business, government, household, or nonprofit organizations
#'
#' This function standardizes messy text data and/or email information to social organizations. 
#' The detect_orgs() function integrates text_to_orgs() and email_to_orgs() together, providing 
#' the capacity to match users to organizations for social, economic, or policy analysis in a tidy framework.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param text Character vector of messy or unstructured text that will be matched to organizations 
#' from one (or all) of five economic sectors (see sector parameter).
#' @param output Output column to be created as string or symbol.
#' @param sector Sector to match by organizations. Currently, the only option is "academic" 
#' with "business", "government", "household", and "nonprofit" in development.
#' @param email Character vector of email or email domain information.
#'
#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified_users <- github_users %>%
#'   detect_orgs(login, company, organization, academic, email)
#'

detect_orgs <- function(data, id, text, output, sector, email){ 
  # TODO: need to add an if clause in the case that email = FALSE
  # 1. convert all vars with enquos
  id <- enquo(id)
  text <- enquo(text)
  output <- enquo(output)
  sector <- enquo(sector)
  email <- enquo(email)
  `%notin%` <- Negate(`%in%`)
  # 2. match by text entries 
  matched_by_text <- data %>%
    tidyorgs::text_to_orgs(!!id, !!text, !!output, !!sector)
  already_classified <- matched_by_text[,1]
  # 3. match by all emails (first by orgs, then by misc)
  matched_by_email <- data %>%
    dplyr::filter(!!id %notin% already_classified) %>%
    tidyorgs::email_to_sectors(!!id, !!email, !!output, !!sector)
  already_classified <- c(already_classified, matched_by_email[,1])
  # 4. load the academic misc. terms 
  academic_terms <- readr::read_rds(file = "R/sector_terms.rds") %>% 
    dplyr::filter(sector_group == "academic")
  academic_terms <- na.omit(academic_terms$terms)
  # 4. match by misc. academic terms 
  matched_by_sector <- data %>%
    dplyr::filter(!!id %notin% already_classified) %>%
    tidytext::unnest_tokens(words, !!text) %>%
    dplyr::filter(words %in% academic_terms) %>%
    dplyr::mutate("{{output}}" := "misc. academic") %>%
    dplyr::distinct(!!id, !!output) 
  # 5. bind all matched data together and add sector 
  all_matched_data <- matched_by_text %>% 
    dplyr::bind_rows(matched_by_sector) %>% 
    dplyr::mutate("{{sector}}" := 1) %>% 
    dplyr::bind_rows(matched_by_email) %>% 
    dplyr::arrange(!!output)
  # 6. join classified data back to the original dataframe
  suppressMessages(
  joined_data <- data %>% 
    dplyr::left_join(all_matched_data) %>% 
    dplyr::mutate("{{sector}}" := tidyr::replace_na(!!sector, 0)) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::group_by(!!id, !!text, !!email, !!sector) %>%
    dplyr::mutate("{{output}}" := paste(!!output, collapse = "|")) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::mutate("{{output}}" := dplyr::na_if(!!output, "NA")) %>% 
    dplyr::ungroup())
  joined_data
}