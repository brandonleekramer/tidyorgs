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
#'   email_to_sectors(login, email, organization, academic)
#'

email_to_sectors <- function(data, id, input, output, sector){ 
  
  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  sector <- enquo(sector)
  `%notin%` <- Negate(`%in%`)
  
  # 2. match by email entries 
  matched_by_email <- data %>%
    email_to_orgs(!!id, !!input, !!output, !!sector)
  already_classified <- matched_by_email[,1]
  
  # 3. load the misc academic email domains 
  academic_domains <- readr::read_rds(file = "R/sector_domains.rds") %>% 
    dplyr::filter(sector_group == "academic")
  academic_domains <- paste(na.omit(academic_domains$domains), collapse = "|")
  
  # 4. match misc academic emails 
  matched_misc <- data %>%
    dplyr::filter(!!id %notin% already_classified) %>%
    # drop missing emails, all domains to lower, extract domain info after @ sign
    tidyr::drop_na(!!input) %>%
    dplyr::mutate("{{ input }}" := tolower(!!input)) %>%
    dplyr::mutate(domain = sub('.*@', '', !!input)) %>%
    # classifies emails with .edu, .ac, etc. as misc. academic 
    # note: clearly room for improvement in non-us (.jp,.cn,.be,.br,.de,.fr, etc.)
    dplyr::filter(grepl("\\.edu|\\.sch|uni-|univ-|alumni|alumno", domain)
                  | grepl(academic_domains, domain)) %>%
    # omitting known nonprofits with academic domains 
    dplyr::filter(!grepl("ucar.edu|academia.edu|ict.ac.cn", domain)) %>%
    dplyr::mutate("{{ output }}" := "misc. academic", "{{ sector }}" := 1) %>%
    dplyr::select(!!id, !!input, !!output, !!sector)
  
  # 5. bind the data together 
  matched_by_both <- matched_by_email %>% 
    dplyr::bind_rows(matched_misc) %>% 
    dplyr::mutate("{{sector}}" := 1) %>% 
    dplyr::select(!!id, !!output, !!sector)
  matched_by_both

}

