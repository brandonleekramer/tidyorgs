#' Match messy text and email data to academic, business, government, household, or nonprofit organizations
#'
#' This function standardizes messy text data and/or email information to social organizations. 
#' The detect_orgs() function iterates through email domains and unstructured text to match patterns in our 
#' curated dictionaries to standardize organizations. This tool is designed to optimize pattern detection 
#' for in the linkage of multiple datasets, for bibliometric analysis, and for sector classification in 
#' social, economic, and policy analysis.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of messy or unstructured text that will be matched to organizations 
#' from one (or all) of five economic sectors (see sector parameter).
#' @param output Output column to be created as string or symbol.
#' @param sector Sector to match by organizations. Currently, the only option is "academic" 
#' with "business", "government", "household", and "nonprofit" in development.
#' @param email Optional character vector of email or email domain information. Defaults to FALSE.
#' @param country Optional parameter that returns country of organization when available. Defaults to FALSE.
#' @param parent_org Optional parameter that returns the parent organization when available. 
#' For the academic sector, this value is the school system of the organization. Defaults to FALSE.
#' @param org_type Optional parameter that returns organization type when available. 
#' Current return values include "Public", "Private for-profit", and "Private not-for-profit". Defaults to FALSE.
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
#' @export
detect_orgs <- function(data, id, input, output, 
                        sector = c("academic", "business"), 
                        email = FALSE, 
                        country = FALSE, 
                        parent_org = FALSE, 
                        org_type = FALSE){ 
  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  #sector <- enquo(sector)
  sector <- rlang::arg_match(sector)
  `%notin%` <- Negate(`%in%`)
  # 2. match by text entries 
  if (sector == "academic") {
   matched_by_text <- data %>%
    text_to_orgs(!!id, !!input, !!output, "academic")
   sector_terms <- sector_terms %>%
     dplyr::filter(sector_group == "academic")
  } else if (sector == "business") {
    matched_by_text <- data %>%
      text_to_orgs(!!id, !!input, !!output, "business")
    sector_terms <- sector_terms %>%
      dplyr::filter(sector_group == "business")
  } else if (sector == "government") {
    matched_by_text <- data %>%
      text_to_orgs(!!id, !!input, !!output, "government")
    sector_terms <- sector_terms %>%
      dplyr::filter(sector_group == "government")
  } else if (sector == "nonprofit") {
    matched_by_text <- data %>%
      text_to_orgs(!!id, !!input, !!output, "nonprofit")
    sector_terms <- sector_terms %>%
      dplyr::filter(sector_group == "nonprofit")
  }
  already_classified <- matched_by_text %>% dplyr::rename(tmp_id = !!id)
  already_classified <- already_classified$tmp_id
  sector_terms <- na.omit(sector_terms$terms)
  
  if (missing(email)) {
  #   # 3a. match by misc. academic terms
    matched_by_sector <- data %>%
      dplyr::filter(!!id %notin% already_classified) %>%
      tidytext::unnest_tokens(words, !!input) %>%
      dplyr::filter(words %in% sector_terms) %>%
      dplyr::mutate("{{output}}" := "Misc. Sector") %>%
      dplyr::distinct(!!id, !!output)
  #   # 4a. bind all matched data together and add sector
    all_matched_data <- matched_by_text %>%
      dplyr::bind_rows(matched_by_sector) %>%
      dplyr::mutate(sector = 1) %>%
      dplyr::arrange(!!output)
  #   # 5a. join classified data back to the original dataframe
    suppressMessages(
      joined_data <- data %>%
        dplyr::left_join(all_matched_data) %>%
        dplyr::mutate(sector = tidyr::replace_na(sector, 0)) %>%
        dplyr::distinct(across(everything())) %>%
        dplyr::group_by(!!id, !!input, sector) %>%
        dplyr::mutate("{{output}}" := paste(!!output, collapse = "|")) %>%
        dplyr::distinct(across(everything())) %>%
        dplyr::mutate("{{output}}" := dplyr::na_if(!!output, "NA")) %>%
        dplyr::ungroup() %>% 
        dplyr::rename("{{sector}}" := sector) %>% 
        dplyr::rename_all(~stringr::str_replace_all(.,"\"",""))) 
   } else {
    email <- enquo(email)
    # 3b. match by all emails (first by orgs, then by misc)
    if (!!sector == "academic") {
      matched_by_email <- data %>%
        dplyr::filter(!!id %notin% already_classified) %>%
        email_to_sectors(!!id, !!email, !!output, "academic") %>%
        filter(!!output != "NA")
    } else if (sector == "business") {
      matched_by_email <- data %>%
        dplyr::filter(!!id %notin% already_classified) %>%
        email_to_sectors(!!id, !!email, !!output, "business") %>%
        filter(!!output != "NA")
    } else if (sector == "government") {
      matched_by_email <- data %>%
        dplyr::filter(!!id %notin% already_classified) %>%
        email_to_sectors(!!id, !!email, !!output, "govnerment") %>%
        filter(!!output != "NA")
    } else if (sector == "nonprofit") {
      matched_by_email <- data %>%
        dplyr::filter(!!id %notin% already_classified) %>%
        email_to_sectors(!!id, !!email, !!output, "nonprofit") %>%
        filter(!!output != "NA")
    }
   }
    newly_classified <- matched_by_email %>% dplyr::rename(tmp_id = !!id)
    newly_classified <- newly_classified$tmp_id
    already_classified <- c(already_classified, newly_classified)
    # 4b. match by misc. academic terms
    matched_by_sector <- data %>%
      dplyr::filter(!!id %notin% already_classified) %>%
      tidytext::unnest_tokens(words, !!input) %>%
      dplyr::filter(words %in% sector_terms) %>%
      dplyr::mutate("{{output}}" := "Misc. Sector") %>%
      dplyr::distinct(!!id, !!output)
    # 5b. bind all matched data together and add sector
    all_matched_data <- matched_by_text %>%
      dplyr::bind_rows(matched_by_sector) %>%
      dplyr::mutate(sector = 1) %>%
      dplyr::bind_rows(matched_by_email) %>%
      dplyr::arrange(!!output)
    # 6b. join classified data back to the original dataframe
    suppressMessages(
      joined_data <- data %>%
        dplyr::left_join(all_matched_data) %>%
        dplyr::mutate(sector = tidyr::replace_na(sector, 0)) %>%
        dplyr::distinct(across(everything())) %>%
        dplyr::group_by(!!id, !!input, !!email, sector) %>%
        dplyr::mutate("{{output}}" := paste(!!output, collapse = "|")) %>%
        dplyr::distinct(across(everything())) %>%
        dplyr::mutate("{{output}}" := dplyr::na_if(!!output, "NA")) %>%
        dplyr::ungroup() %>%
        dplyr::rename("{{sector}}" := sector) %>%
        dplyr::rename_all(~stringr::str_replace_all(.,"\"",""))
      )
  # # adding basic covariates for academic sector
  suppressMessages(
  if(country == TRUE || parent_org == TRUE || org_type == TRUE){
    #academic_institutions <- tidyorgs::academic_institutions %>%
    academic_institutions <- academic_institutions %>%
      dplyr::mutate("{{output}}" := organization_name) %>%
      dplyr::select(!!output, country, parent_org, org_type)

    if(country == TRUE && parent_org == TRUE && org_type == TRUE){ # TTT
      joined_data <- joined_data %>% dplyr::left_join(academic_institutions)
    } else if(country == FALSE && parent_org == TRUE && org_type == TRUE){ # FTT
      joined_data <- joined_data %>% dplyr::left_join(academic_institutions %>%
                           dplyr::select(!!output, parent_org, org_type))
    } else if(country == TRUE && parent_org == FALSE && org_type == TRUE){ # TFT
      joined_data <- joined_data %>% dplyr::left_join(academic_institutions %>%
                           dplyr::select(!!output, country, org_type))
    } else if(country == TRUE && parent_org == TRUE && org_type == FALSE){ # TTF
      joined_data <- joined_data %>% dplyr::left_join(academic_institutions %>%
                           dplyr::select(!!output, country, parent_org))
    } else if(country == TRUE && parent_org == FALSE && org_type == FALSE){ # TFF
      joined_data <- joined_data %>% dplyr::left_join(academic_institutions %>%
                           dplyr::select(!!output, country))
    } else if(country == FALSE && parent_org == FALSE && org_type == TRUE){ # FFT
      joined_data <- joined_data %>% dplyr::left_join(academic_institutions %>%
                           dplyr::select(!!output, parent_org))
    } else if(country == FALSE && parent_org == TRUE && org_type == FALSE){ # FTF
      joined_data <- joined_data %>% dplyr::left_join(academic_institutions %>%
                           dplyr::select(!!output, parent_org))
    } else { joined_data }
  } else { joined_data }
  )
}