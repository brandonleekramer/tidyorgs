#' Match emails to social organizations
#'
#' This function matches users to organizations based on their email domains and/or 
#' sub-domains. For example, users with domains ending in "virginia.edu" will be 
#' matched to the "university of virginia". This function has been integrated as an 
#' optional parameter alongside the email_to_sectors() function in the detect_orgs() function.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of emails or email domains that will be matched to 
#' organizations from one (or all) of five economic sectors (see sector parameter).
#' @param output Output column to be created as string or symbol.
#' @param sector Sector to match by organizations. Currently, the only option is "academic" 
#' with "business", "government", "household", and "nonprofit" in development.
#'
#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified_by_email <- github_users %>%
#'   email_to_orgs(login, email, organization, academic)
#'
#' @export
email_to_orgs <- function(data, id, input, output, 
                          sector = c("academic", "business", 
                                     "government", "nonprofit")){ 
  # 1. pull in all academic email domains
  #academic_dictionary <- tidyorgs::academic_institutions
  if (sector == "academic") {
    sector_dictionary <- academic_institutions
  } else if (sector == "business") { 
    sector_dictionary <- business_data
  } else if (sector == "government") { 
    sector_dictionary <- government_data
  } else if (sector == "nonprofit") { 
    sector_dictionary <- nonprofit_data
  }

  sector_dictionary <- sector_dictionary %>%
    tidyr::unnest_legacy(domains = strsplit(domains, "\\|")) %>%
    tidyr::drop_na(domains) %>%
    dplyr::select(domains, organization_name) %>%
    dplyr::rename(org_domain = domains)
  sector_vector <- na.omit(sector_dictionary$org_domain)
  sector_deframed <- sector_dictionary %>%
    dplyr::mutate(beginning = "\\b(?i)((?<!.)", ending = ")\\b",
                  org_domain = paste0(beginning, org_domain, ending)) %>%
    dplyr::select(org_domain, organization_name) %>% tibble::deframe()
  # 2. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  sector <- enquo(sector)
  `%notin%` <- Negate(`%in%`)
  # 3. drop missing emails, all domains to lower, extract domain info after @ sign
  all_domains_df <- data %>%
    tidyr::drop_na(!!input) %>%
    dplyr::mutate("{{ input }}" := tolower(!!input)) %>%
    dplyr::mutate(domain = sub('.*@', '', !!input))
  # 4. matches all of the root domains based on dictionary
  emails_df <- all_domains_df %>%
    dplyr::filter(domain %in% sector_vector)
  # 5. remove all of the sub-domains and match again on roots
  emails_df <- all_domains_df %>%
    dplyr::filter(stringr::str_count(domain, "[.]") == 2) %>%
    dplyr::mutate(domain = sub("^.*?\\.", '', domain)) %>%
    dplyr::filter(domain %in% sector_vector) %>%
    dplyr::bind_rows(emails_df)
  # 5. add institution names based on the classified email information
  df <- emails_df %>%
    dplyr::mutate("{{ output }}" := stringr::str_replace_all(
      domain, sector_deframed)) %>%
    dplyr::select(!!id, !!output)
  df
}