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

  # match by text entries first
  matched_by_text <- data %>%
    match_orgs_by_text(!!id, !!text, !!output, !!sector)
  already_classified <- matched_by_text[,1]

  # match by emails second
  matched_by_email <- github_users %>%
    filter(!!id %notin% already_classified) %>%
    match_orgs_by_email(!!id, !!email, !!output, !!sector)

  matched_by_both <- matched_by_text %>%
    dplyr::bind_rows(matched_by_email)

  matched_by_both
  #already_classified <- c(already_classified, matched_by_text[,1])
  # next need to add all the misc classifications
  # add join parameter that defaults to TRUE
  # need to update email to default = FALSE

}






