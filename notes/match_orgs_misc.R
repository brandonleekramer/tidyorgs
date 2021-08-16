#' Match messy text and email data to social organizations
#'
#' Full explanation here... Combines match_orgs_by_email and match_orgs_by_text
#'
#' @param data A data frame.
#' @param id A numeric or character vector unique to each entry.
#' @param text Character vector of messy or unstructed text that will
#' be unnested as n-grams and matched to dictionary of academic instiutions.
#' @param output Desired name of organization column.
#' @param sector Choose "all", "academic", "business", "goverment", or "nonprofit". Defaults to "all".
#' @param email Character vector of emails or email domains.
#'

match_orgs_to_sector <- function(data, id, text, output, sector, email){

  id <- enquo(id)
  text <- enquo(text)
  output <- enquo(output)
  sector <- enquo(sector)
  email <- enquo(email)
  `%notin%` <- Negate(`%in%`)

  misc_academic_df <- df_to_parse %>%
    tidytable::filter.(!!id %notin% already_classified) %>%
    tidytext::unnest_tokens(tmp_words, !!input) %>%
    tidytable::filter.(tmp_words %in% c("student", "academic", "academia", "college",
                                        "bootcamp", "teacher", "professor",
                                        "university", "universidad", "universitat")) %>%
    tidytable::mutate.(words = "misc. academic", academic = 1) %>%
    tidytable::select.(!!id, words, academic)

  # the equivalent of this in the business sector is using all of daniel's business words

  # nonprofit variations of 501c, charity, charitable, fund, foundation, etc

  # househould matches go in here as well


}
