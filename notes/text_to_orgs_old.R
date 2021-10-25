#' Match messy text data to social organizations using a 'funneling' method
#'
#' This function matches unstructured text data to various dictionaries of 
#' organizations by extracting and iterating through consecuetive word sequences 
#' (or n-grams). To do this, the function extracts n-grams using the tidytext 
#' package, matching all sequences in the unstructured data that have n words 
#' and then 'funneling' through all sequences of n-1, n-2, etc. words before 
#' matching the single tokens. This process returns a dataframe of ids, 
#' organizations, and sectors for only those rows matched within the sectors specified.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of messy or unstructured text that will
#' be unnested as n-grams and matched to dictionary of organizations in specified sector.
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
#' classified_by_text <- github_users %>%
#'   text_to_orgs(login, company, organization, academic)
#'
#' @export
text_to_orgs_old <- function(data, id, input, output, sector
                               #sector = c("all", "academic", "business", "government", "nonprofit")
                               ){
  # to update: this beginning part can just be a helper function
  # that i can use at the beginning of each function
  # 1. convert all vars with enquos and check for errors
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  sector <- enquo(sector)
  `%notin%` <- Negate(`%in%`)
  # 2. pull in academic institutions dictionary 
  #dictionary <- tidyorgs::academic_institutions
  dictionary <- academic_institutions
  #dictionary <- readr::read_rds(file = "R/academic_institutions.rds")
  ids_to_filter <- c("nonexistent-user")
  funnelized <- data.frame()
  # 3. standardize common academic instiution terms 
  data <- data %>%
    tidyr::drop_na(!!input) %>%
    dplyr::mutate("{{input}}" := tolower(!!input),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(univ\\.)\\b", "university"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(univesity)\\b", "university"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(univeristy)\\b", "university"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(universoty)\\b", "university"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(universit)\\b", "université"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(universitt)\\b", "universität"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(technolgy)\\b", "technology"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(simn bolvar)\\b", "simón bolívar"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(mnster)\\b", "münster"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(king's)\\b", "kings"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(queen's)\\b", "queens"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(xi'an jiaotong)\\b", "xian jiaotong"),
                  "{{input}}" := stringr::str_replace(!!input, "a & m", "a&m"),
                  "{{input}}" := stringr::str_replace(!!input, "@", ""),
                  "{{input}}" := stringr::str_replace(!!input, " & ", " and "))
  # 4. use a for loop to funnel match n-grams of lengths 2-12 
  for (n_word in 12:2) {
    # note: 12 is an arbitrary number that will eventually correspond to largest n in dictionary
    subdictionary <- dictionary %>%
      tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
      dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
      dplyr::filter(word_count == n_word)
    subdictionary <- na.omit(subdictionary$catch_terms)
    funnelized <- data %>%
      dplyr::filter(!!id %notin% ids_to_filter) %>%
      tidytext::unnest_tokens(words, !!input, token="ngrams", n=n_word, to_lower = TRUE) %>%
      dplyr::filter(words %in% subdictionary) %>%
      dplyr::mutate("{{sector}}" := 1) %>%
      dplyr::filter(!!sector == 1) %>%
      dplyr::select(!!id, words, !!sector) %>%
      dplyr::bind_rows(funnelized)
    newly_classified <- funnelized[,1]
    ids_to_filter <- c(ids_to_filter, newly_classified)
  }
  # 5. funnel match on all of the single tokens 
  subdictionary <- dictionary %>%
    tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
    dplyr::filter(word_count == 1)
  subdictionary <- na.omit(subdictionary$catch_terms)
  funnelized <- data %>%
    dplyr::filter(!!id %notin% ids_to_filter) %>%
    tidytext::unnest_tokens(words, !!input) %>%
    dplyr::filter(words %in% subdictionary) %>%
    dplyr::mutate("{{sector}}" := 1) %>%
    dplyr::select(!!id, words, !!sector) %>%
    dplyr::bind_rows(funnelized) 
  # 6. standardize all of the organizations 
  #dictionary <- tidyorgs::academic_institutions %>%
  dictionary <- academic_institutions %>%
    dplyr::mutate(beginning = "\\b(?i)(", ending = ")\\b",
                  recode_column = paste0(beginning, recode_column, ending)) %>%
    dplyr::select(recode_column, organization_name) %>% tibble::deframe()
  standardized <- funnelized %>%
    dplyr::mutate("{{output}}" := tolower(words)) %>%
    dplyr::mutate("{{output}}" := stringr::str_replace_all({{ output }}, dictionary)) %>%
    dplyr::select(!!id, !!output)
  standardized
}

