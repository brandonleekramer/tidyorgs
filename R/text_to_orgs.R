#' Match messy text data to social organizations using a 'funneling' method
#'
#' This function matches unstructured text data to a dictionary of 9,000+
#' academic, business, government, and nonprofit institutions by extracting
#' and iterating through consequetive word sequences (or n-grams). To do this,
#' the function extracts n-grams, matching all sequences in the unstructured
#' data that have 12 words (default) and then 'funneling' through all sequences
#' of 9, 8, etc. words before matching the single tokens. This process returns
#' a dataframe of ids, organizations, and sectors for only those rows matched
#' within the sector(s) specified.
#'
#' @param data A data frame.
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of messy or unstructed text that will
#' be unnested as n-grams and matched to dictionary of academic instiutions.
#' @param output Desired name of classified organization column.
#' @param sector Choose "all", "academic", "business", "goverment", or "nonprofit". Defaults to "academic".
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

text_to_orgs <- function(data, id, input, output, sector
                               #sector = c("all", "academic", "business", "government", "nonprofit")
                               ){

  # to update: this beginning part can just be a helper function
  # that i can use at the beginning of each function
  # NOTE it might be better to create if () else if () etc on one variable,
  # with the missing, logical, numeric, depreciated, then move onto the next variable after
  if (missing(id)) { # |is_logical(id)
    "error: id requires numeric or character vector"
  } else if (missing(input)) { # |is_logical(input)|is_numeric(input)
    "error: input requires character vector"
  } else if (missing(sector)) {
    "error: no sector parameter was specified"
  }

  #sector <- rlang::arg_match(sector)
  # still need to add in all of the dictionaries and the arg_match here

  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  sector <- enquo(sector)
  `%notin%` <- Negate(`%in%`)

  # 2. pull in academic institutions dictionary 
  dictionary <- readr::read_rds(file = "R/academic_instiutions.rds")
  ids_to_filter <- c("nonexistent-user")
  funnelized <- data.frame()

  # 3. standardize common academic instiution terms 
  data <- data %>%
    tidyr::drop_na(!!input) %>%
    dplyr::mutate("{{input}}" := tolower(!!input),
                  "{{input}}" := stringr::str_replace(!!input, "univ\\.", "university"),
                  "{{input}}" := stringr::str_replace(!!input, "univesity", "university"),
                  "{{input}}" := stringr::str_replace(!!input, "univeristy", "university"),
                  "{{input}}" := stringr::str_replace(!!input, "universoty", "university"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(universit)\\b", "université"),
                  "{{input}}" := stringr::str_replace(!!input, "\\b(?i)(universitt)\\b", "universität"),
                  "{{input}}" := stringr::str_replace(!!input, "a & m", "a&m"))

  # 4. use a for loop to funnel match n-grams of lengths 2-12 
  for (n_word in 12:2) {
    # note: 12 is an arbitrary number that will eventually correspond to largest n in dictionary
    subdictionary <- dictionary %>%
      tidyr::unnest_legacy(academic_terms = base::strsplit(academic_terms, "\\|")) %>%
      dplyr::mutate(word_count = lengths(base::strsplit(academic_terms, "\\W+"))) %>%
      dplyr::filter(word_count == n_word)
    subdictionary <- na.omit(subdictionary$academic_terms)

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
    tidyr::unnest_legacy(academic_terms = base::strsplit(academic_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(academic_terms, "\\W+"))) %>%
    dplyr::filter(word_count == 1)
  subdictionary <- na.omit(subdictionary$academic_terms)
  funnelized <- data %>%
    dplyr::filter(!!id %notin% ids_to_filter) %>%
    tidytext::unnest_tokens(words, !!input) %>%
    dplyr::filter(words %in% subdictionary) %>%
    dplyr::mutate("{{sector}}" := 1) %>%
    dplyr::select(!!id, words, !!sector) %>%
    dplyr::bind_rows(funnelized) %>%
    tidyorgs::standardize_orgs(words, !!output, !!sector) %>%
    select(!!id, !!output)
  funnelized

}

