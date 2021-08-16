#' Standardize organizations names across economic sectors
#'
#' Full explanation here...
#'
#' @param data A data frame.
#'
#' @param input Character vector of text that contains unstandardized organization information
#'
#' @param output Desired name of organization column.
#'
#' @param sector Choose "academic", "all", "business", "goverment", or "nonprofit".
#'
#'
#'
#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified <- github_users %>%
#'   standardize_orgs(login, company, academic)
#'
#'

standardize_orgs <- function(df, input, output, sector){

  input <- enquo(input)
  output <- enquo(output)

  dictionary <- readr::read_rds(file = "R/academic_instiutions.rds") %>%
    dplyr::mutate(beginning = "\\b(?i)(", ending = ")\\b",
                  original_string = paste0(beginning, original_string, ending)) %>%
    dplyr::select(original_string, new_string) %>% tibble::deframe()

  df <- df %>%
    dplyr::mutate("{{output}}" := tolower({{ input }})) %>%
    dplyr::mutate("{{output}}" := stringr::str_replace_all({{ output }}, dictionary))
  df

}
