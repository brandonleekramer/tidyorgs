#' Match messy text and email data to social organizations
#'
#' Full explanation here... Combines text_to_org and email_to_org
#'
#' @param data A data frame.
#' @param id A numeric or character vector unique to each entry.
#' @param text Character vector of messy or unstructed text that will
#' be unnested as n-grams and matched to dictionary of academic instiutions.
#' @param output Desired name of organization column.
#' @param sector Choose "all", "academic", "business", "goverment", or "nonprofit". Defaults to "all".
#' @param email Character vector of emails or email domains.
#'
#'#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified_users <- github_users %>%
#'   detect_orgs(login, company, organization, academic, email)
#'

detect_orgs <- function(data, id, text, output, sector, email){ 
  # need to update email to default = FALSE
  
  print(paste("Started detect_orgs() at", Sys.time()))
  
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
  
  all_matched_data <- matched_by_text %>% 
    dplyr::bind_rows(matched_by_sector) %>% 
    dplyr::mutate("{{sector}}" := 1) %>% 
    dplyr::bind_rows(matched_by_email) %>% 
    dplyr::arrange(!!output)
  
  # todo: need to add an if clause in the case that email = FALSE
  df <- data %>% 
    dplyr::left_join(all_matched_data) %>% 
    dplyr::mutate("{{sector}}" := tidyr::replace_na(!!sector, 0)) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::group_by(!!id, !!text, !!email, !!sector) %>%
    dplyr::mutate("{{output}}" := paste(!!output, collapse = "|")) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::mutate("{{output}}" := dplyr::na_if(!!output, "NA"))
  df
}






