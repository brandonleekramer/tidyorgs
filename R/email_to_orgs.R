#' Match emails to social organizations
#'
#' Full explanation here...
#'
#' @param data A data frame.
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of emails or email domains.
#' @param output Desired name of classified organization column.
#' @param sector Choose "all", "academic", "business", "goverment", or "nonprofit". Defaults to "academic".
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

email_to_orgs <- function(data, id, input, output, sector){ 

  academic_dictionary <- readr::read_rds(file = "R/academic_instiutions.rds")
  academic_domains <- academic_dictionary %>%
    tidyr::unnest_legacy(domains = strsplit(domains, "\\|")) %>%
    tidyr::drop_na(domains) %>%
    dplyr::select(domains, new_string) %>%
    dplyr::rename(org_domain = domains, org_name = new_string)
  academic_vector <- na.omit(academic_domains$org_domain)
  academic_deframed <- academic_domains %>%
    dplyr::mutate(beginning = "\\b(?i)((?<!.)", ending = ")\\b",
                       org_domain = paste0(beginning, org_domain, ending)) %>%
    dplyr::select(org_domain, org_name) %>% tibble::deframe()

  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  sector <- enquo(sector)
  `%notin%` <- Negate(`%in%`)

  # drop missing emails, all domains to lower, extract domain info after @ sign
  all_domains_df <- data %>%
    tidyr::drop_na(!!input) %>%
    dplyr::mutate("{{ input }}" := tolower(!!input)) %>%
    dplyr::mutate(domain = sub('.*@', '', !!input))

  # matches all of the root domains based on dictionary
  emails_df <- all_domains_df %>%
    dplyr::filter(domain %in% academic_vector)

  # remove all of the subdomains and match again on roots
  emails_df <- all_domains_df %>%
    dplyr::filter(str_count(domain, "[.]") == 2) %>%
    dplyr::mutate(domain = sub("^.*?\\.", '', domain)) %>%
    dplyr::filter(domain %in% academic_vector) %>%
    dplyr::bind_rows(emails_df)

  # add institution names based on the classified email information
  # this essentially uses str_replace_all() to recode all domains into institutions
  df <- emails_df %>%
    dplyr::mutate("{{ output }}" := stringr::str_replace_all(domain, academic_deframed)) %>%
    dplyr::select(!!id, !!output)
  df

}
