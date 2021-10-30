#' Probabilistically match emails to an economic sector 
#'
#' This function assigns entries into a selected economic sector by 
#' probabilistically matching common domains and sub-domains like ".edu" and 
#' ".co.uk". Matched entries return "misc." with the appropriate sector name 
#' in the output column. This function has been integrated as an optional parameter 
#' alongside the email_to_orgs() function in the detect_orgs() function.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of emails or email domains that will 
#' be matched to the economic sector.
#' @param output Output column to be created as string or symbol.
#' @param sector Sector to match emails. Currently, the only option is "academic" 
#' with "business", "government", "household", and "nonprofit" in development.
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
#' @export
email_to_sectors <- function(data, id, input, output, 
                             sector = c("academic", "business", 
                                        "government", "nonprofit")
                             ){ 
  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  sector <- rlang::arg_match(sector)
  `%notin%` <- Negate(`%in%`)
  # 2. match by email entries 
  if (sector == "academic") {
  matched_by_email <- data %>%
    email_to_orgs(!!id, !!input, !!output, "academic")
  already_classified <- matched_by_email[,1]
  # 3. load the misc academic email domains 
  academic_domains <- tidyorgs::sector_domains %>% 
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
    dplyr::mutate("{{ output }}" := "Misc. Academic", sector = 1) %>%
    dplyr::select(!!id, !!input, !!output, sector)
  # 5. bind the data together 
  matched_by_both <- matched_by_email %>% 
    dplyr::bind_rows(matched_misc) %>% 
    dplyr::mutate(sector = 1) %>% 
    dplyr::select(!!id, !!output, sector) #%>% 
    #dplyr::rename("{{sector}}" := sector) %>% 
    #dplyr::rename_all(~stringr::str_replace_all(.,"\"","")) 
  # 6. standardize all of the organizations
  matched_by_both
  } else if (sector == "business") {
    matched_by_email <- data %>%
      email_to_orgs(!!id, !!input, !!output, "business")
    already_classified <- matched_by_email[,1]
    # 3. load the misc business email domains 
    business_domains <- tidyorgs::sector_domains %>% 
      dplyr::filter(sector_group == "business")
    business_domains <- paste(na.omit(business_domains$domains), collapse = "|")
    # 4. match misc academic emails 
    matched_misc <- data %>%
      dplyr::filter(!!id %notin% already_classified) %>%
      # drop missing emails, all domains to lower, extract domain info after @ sign
      tidyr::drop_na(!!input) %>%
      dplyr::mutate("{{ input }}" := tolower(!!input)) %>%
      dplyr::mutate(domain = sub('.*@', '', !!input)) %>%
      # classifies emails with .edu, .ac, etc. as misc. academic 
      # note: clearly room for improvement in non-us (.jp,.cn,.be,.br,.de,.fr, etc.)
      dplyr::filter(#grepl("\\.co(?!m)", domain) | 
                      grepl(business_domains, domain)) %>%
      # omitting known nonprofits with academic domains 
      #dplyr::filter(!grepl("ucar.edu|academia.edu|ict.ac.cn", domain)) %>%
      dplyr::mutate("{{ output }}" := "Misc. Business", sector = 1) %>%
      dplyr::select(!!id, !!input, !!output, sector)
    # 5. bind the data together 
    matched_by_both <- matched_by_email %>% 
      dplyr::bind_rows(matched_misc) %>% 
      dplyr::mutate(sector = 1) %>% 
      dplyr::select(!!id, !!output, sector) 
  } else if (sector == "government") {
    
    matched_by_email <- data %>%
      email_to_orgs(!!id, !!input, !!output, "government")
    already_classified <- matched_by_email[,1]
    # 3. load the misc business email domains 
    government_domains <- sector_domains %>% 
      dplyr::filter(sector_group == "government")
    government_domains <- paste(na.omit(government_domains$domains), collapse = "|")
    # 4. match misc academic emails 
    matched_misc <- data %>%
      dplyr::filter(!!id %notin% already_classified) %>%
      # drop missing emails, all domains to lower, extract domain info after @ sign
      tidyr::drop_na(!!input) %>%
      dplyr::mutate("{{ input }}" := tolower(!!input)) %>%
      dplyr::mutate(domain = sub('.*@', '', !!input)) %>%
      # classifies emails with .edu, .ac, etc. as misc. academic 
      # note: clearly room for improvement in non-us (.jp,.cn,.be,.br,.de,.fr, etc.)
      dplyr::filter(grepl("\\.gov|\\.gob|\\.mil", domain) 
                    | grepl(government_domains, domain)) %>%
      # omitting known nonprofits with academic domains 
      #dplyr::filter(!grepl("ucar.edu|academia.edu|ict.ac.cn", domain)) %>%
      dplyr::mutate("{{ output }}" := "Misc. Government", sector = 1) %>%
      dplyr::select(!!id, !!input, !!output, sector)
    # 5. bind the data together 
    matched_by_both <- matched_by_email %>% 
      dplyr::bind_rows(matched_misc) %>% 
      dplyr::mutate(sector = 1) %>% 
      dplyr::select(!!id, !!output, sector)

  } else if (sector == "nonprofit") {
    
    matched_by_email <- data %>%
      email_to_orgs(!!id, !!input, !!output, "nonprofit")
    already_classified <- matched_by_email[,1]
    # 3. load the misc business email domains 
    nonprofit_domains <- sector_domains %>% 
      dplyr::filter(sector_group == "nonprofit")
    nonprofit_domains <- paste(na.omit(nonprofit_domains$domains), collapse = "|")
    # 4. match misc academic emails 
    matched_misc <- data %>%
      dplyr::filter(!!id %notin% already_classified) %>%
      # drop missing emails, all domains to lower, extract domain info after @ sign
      tidyr::drop_na(!!input) %>%
      dplyr::mutate("{{ input }}" := tolower(!!input)) %>%
      dplyr::mutate(domain = sub('.*@', '', !!input)) %>%
      # classifies emails with .edu, .ac, etc. as misc. academic 
      # note: clearly room for improvement in non-us (.jp,.cn,.be,.br,.de,.fr, etc.)
      dplyr::filter(grepl("\\.orgz", domain) | grepl(nonprofit_domains, domain)) %>%
      # omitting known nonprofits with academic domains 
      #dplyr::filter(!grepl("ucar.edu|academia.edu|ict.ac.cn", domain)) %>%
      dplyr::mutate("{{ output }}" := "Misc. Non-Profit", sector = 1) %>%
      dplyr::select(!!id, !!input, !!output, sector)
    # 5. bind the data together 
    matched_by_both <- matched_by_email %>% 
      dplyr::bind_rows(matched_misc) %>% 
      dplyr::mutate(sector = 1) %>% 
      dplyr::select(!!id, !!output, sector)
  }
}