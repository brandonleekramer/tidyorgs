#' Match messy text data to countries using a 'funneling' method
#'
#' This function allocates entries of unstructured text data to countries 
#' extracting and iterating through consecuetive word sequences (or n-grams). 
#' To do this, the function extracts n-grams using the tidytext 
#' package, matching all sequences in the unstructured data that have n words 
#' and then 'funneling' through all sequences of n-1, n-2, etc. words before 
#' matching the single tokens. This function returns a dataframe akin to the 
#' original dataframe with one additional column aligning with the "output" parameter.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of messy or unstructured text that will
#' be unnested as n-grams and matched to dictionary of organizations in specified sector.
#' @param output Output column to be created as string or symbol.
#'
#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified_by_text <- github_users %>%
#'   text_to_countries(login, location, country_name)
#'
#' @export
text_to_countries <- function(data, id, input, output){
  # to update: this beginning part can just be a helper function
  # that i can use at the beginning of each function
  # NOTE it might be better to create if () else if () etc on one variable,
  # with the missing, logical, numeric, depreciated, then move onto the next variable after
  if (missing(id)) { # |is_logical(id)
    "error: id requires numeric or character vector"
  } else if (missing(input)) { # |is_logical(input)|is_numeric(input)
    "error: input requires character vector"
  } 
  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  `%notin%` <- Negate(`%in%`)
  # 2. pull in countries dictionary 
  dictionary <- tidyorgs::countries_data
  ids_to_filter <- c("nonexistent-user")
  funnelized <- data.frame()
  # 3. drop missing, convert to lower case, standardize some words 
  data <- data %>%
    tidyr::drop_na(!!input) %>%
    dplyr::mutate("{{input}}" := tolower(!!input),
                  "{{input}}" := stringr::str_replace(!!input, "/", " "),
                  "{{input}}" := stringr::str_replace(!!input, "\\.", " "),
                  "{{input}}" := stringr::str_replace(!!input, "·", " "),
                  "{{input}}" := stringr::str_replace(!!input, "u\\.s\\.", "united states"),
                  "{{input}}" := stringr::str_replace(!!input, "u\\.s\\.a\\.", "united states"),
                  "{{input}}" := sub('p\\.r\\.china', 'china', !!input),
                  "{{input}}" := sub("china's", 'china', !!input),
                  "{{input}}" := sub("·", ' ', !!input),
                  "{{input}}" := sub("中国", 'china', !!input),
                  "{{input}}" := sub("北京市", 'beijing', !!input),
                  "{{input}}" := sub("北京", 'beijing', !!input),
                  "{{input}}" := sub("上海", 'shanghai', !!input),
                  "{{input}}" := sub("广东", 'guangdong', !!input),
                  "{{input}}" := sub("深圳", 'shenzhen', !!input),
                  "{{input}}" := sub("广州", 'guangzhou', !!input),
                  "{{input}}" := sub("四川", 'sichuan', !!input),
                  "{{input}}" := sub("杭州", 'hangzhou', !!input),
                  "{{input}}" := sub("浙江", 'zhejiang', !!input),
                  "{{input}}" := sub("成都", 'chengdu', !!input),
                  "{{input}}" := sub("朝阳", 'chaoyang', !!input),
                  "{{input}}" := sub("湖北", 'hubei', !!input),
                  "{{input}}" := sub("武汉", 'wuhan', !!input),
                  "{{input}}" := sub("江苏", 'jiangsu', !!input),
                  "{{input}}" := sub("南京", 'nanjing', !!input),
                  "{{input}}" := sub("湖南", 'hunan', !!input),
                  "{{input}}" := sub("长沙", 'changsha', !!input),
                  "{{input}}" := sub("海 淀", 'haidian', !!input),
                  "{{input}}" := sub("浙江 杭州", 'hangzhou zhejiang', !!input),
                  "{{input}}" := sub('广州 市', 'guangzhou city china', !!input),
                  "{{input}}" := sub('海 淀', 'haiden china', !!input),
                  "{{input}}" := sub("淀 区", 'dian district china', !!input),
                  "{{input}}" := sub("新 区", 'new district china', !!input),
                  "{{input}}" := sub("省", 'province', !!input),
                  "{{input}}" := sub("区", 'district', !!input),
                  "{{input}}" := sub("海", 'sea', !!input),
                  "{{input}}" := sub("市", 'city', !!input),
                  "{{input}}" := sub('санкт петербург', 'saint petersburg russia', !!input))
  
  # 4. use a for loop to funnel match n-grams of lengths 2-12 
  for (n_word in 6:2) {
    # note: 6 is the longest string in the location data (as of 08-25-2021)
    subdictionary <- dictionary %>%
      tidyr::unnest_legacy(geographic_terms = base::strsplit(catch_terms, "\\|")) %>%
      dplyr::mutate(word_count = lengths(base::strsplit(geographic_terms, "\\W+"))) %>%
      dplyr::filter(word_count == n_word)
    subdictionary <- na.omit(subdictionary$geographic_terms)
    funnelized <- data %>%
      dplyr::filter(!!id %notin% ids_to_filter) %>%
      tidytext::unnest_tokens(words, !!input, token="ngrams", n=n_word, to_lower = TRUE) %>%
      dplyr::filter(words %in% subdictionary) %>%
      dplyr::select(!!id, words) %>%
      dplyr::bind_rows(funnelized)
    newly_classified <- funnelized[,1]
    ids_to_filter <- c(ids_to_filter, newly_classified)
  }
  # 5. funnel match on all of the single tokens 
  subdictionary <- dictionary %>%
    tidyr::unnest_legacy(geographic_terms = base::strsplit(catch_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(geographic_terms, "\\W+"))) %>%
    dplyr::filter(word_count == 1)
  subdictionary <- na.omit(subdictionary$geographic_terms)
  funnelized <- data %>%
    dplyr::filter(!!id %notin% ids_to_filter) %>%
    tidytext::unnest_tokens(words, !!input) %>%
    dplyr::filter(words %in% subdictionary) %>%
    dplyr::select(!!id, words) %>%
    dplyr::bind_rows(funnelized) %>% 
    dplyr::select(!!id, words) 
  dictionary <- tidyorgs::countries_data %>%
    dplyr::mutate(original_string = paste0("\\b(?i)(", recode_column, ")\\b")) %>%
    dplyr::select(original_string, !!output) %>% tibble::deframe()
  finalized <- funnelized %>%
    dplyr::mutate("{{output}}" := stringr::str_replace_all(words, dictionary)) %>% 
    dplyr::select(!!id, !!output) 
  finalized <- finalized %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::group_by(!!id, !!output) %>%
    dplyr::mutate("{{output}}" := paste(!!output, collapse = "|")) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::mutate("{{output}}" := dplyr::na_if(!!output, "NA"))
  finalized
}