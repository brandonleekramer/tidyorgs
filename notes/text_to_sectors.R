#' Probabilistically match text to an economic sector 
#'
#' This function assigns entries into a selected economic sector by 
#' probabilistically matching common terms like "student" (for academic), 
#' "inc" (for business), and "government" (for government). Matched entries 
#' return "misc." with the appropriate sector name in the output column. 
#' This function has been integrated as an optional parameter 
#' alongside the text_to_orgs() function in the detect_orgs() function.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of messy or unstructured text that will be matched to sector.
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
#'   text_to_sectors(login, company, organization, academic)
#'
#' @export
text_to_sectors <- function(data, id, input, output, sector
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
  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  sector <- enquo(sector)
  `%notin%` <- Negate(`%in%`)
  # 2. match by text entries 
  matched_by_text <- data %>%
    tidyorgs::text_to_orgs(!!id, !!input, !!output, !!sector)
  already_classified <- matched_by_text[,1]
  # 3. load the academic misc. terms 
  sector_terms <- dictionary <- tidyorgs::sector_terms %>% 
    dplyr::filter(sector_group == "academic")
  academic_terms <- na.omit(sector_terms$terms)
  # 4. match by misc. academic terms 
  matched_by_sector <- data %>%
    filter(!!id %notin% already_classified) %>%
    tidytext::unnest_tokens(words, !!input) %>%
    dplyr::filter(words %in% sector_terms) %>%
    dplyr::mutate("{{output}}" := "Misc. Academic") %>%
    dplyr::distinct(!!id, !!output)
  # 5. bind the datasets together and add the sector 
  matched_by_both <- matched_by_text %>% 
    dplyr::bind_rows(matched_by_sector) %>% 
    dplyr::mutate("{{sector}}" := 1)
  matched_by_both
}