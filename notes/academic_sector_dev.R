
# academic_dict_terms()
# NOTE: should be obsolete in pkg (now funnel_match)
academic_dict_terms <- function(n_words){

  suppressMessages(library("dplyr"))
  suppressMessages(library("readr"))
  suppressMessages(library("tidyr"))

  n_words <- enquo(n_words)
  setwd("~/git/oss-2020/docs/")
  academic_institutions <- readr::read_csv("academic_institutions.csv", col_types = cols())
  academic_institutions <- academic_institutions %>%
    tidyr::unnest_legacy(academic_terms = strsplit(academic_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(strsplit(academic_terms, "\\W+"))) %>%
    dplyr::filter(word_count == {{ n_words }})
  academic_institutions <- na.omit(academic_institutions$academic_terms)
  academic_institutions
}

# funnel_n_terms()
# NOTE: should be obsolete in pkg (now funnel_match)
funnel_ngrams <- function(df, id, input, category, n_grams, n_words){

  suppressMessages(library("dplyr"))
  suppressMessages(library("tidyr"))
  suppressMessages(library("tidytext"))
  suppressMessages(library("tidytable"))
  `%notin%` <- Negate(`%in%`)

  id <- enquo(id)
  input <- enquo(input)
  category <- enquo(category)

  n_terms_df <- df %>%
    tidytext::unnest_tokens(words, !!input, token="ngrams", n=n_grams) %>%
    tidytable::filter.(words %in% n_words) %>%
    tidytable::mutate.("{{category}}" := 1) %>%
    tidytable::filter.(!!category == 1) %>%
    tidytable::select.(!!id, words, !!category)
  n_terms_df
}

# academic_email_to_org()
academic_email_to_org <- function(df, id, input, output){

  suppressMessages(library("dplyr"))
  suppressMessages(library("readr"))
  suppressMessages(library("tidyr"))
  suppressMessages(library("tidytable"))

  setwd("~/git/oss-2020/docs/")
  academic_institutions <- readr::read_csv("academic_institutions.csv", col_types = cols())
  academic_domains <- academic_institutions %>%
    tidyr::unnest_legacy(domains = strsplit(domains, "\\|")) %>%
    tidytable::drop_na.(domains) %>%
    tidytable::select.(domains, new_string) %>%
    tidytable::rename.(org_domain = domains, org_name = new_string)
  academic_vector <- na.omit(academic_domains$org_domain)
  academic_deframed <- academic_domains %>%
    tidytable::mutate.(beginning = "\\b(?i)((?<!.)", ending = ")\\b",
            org_domain = paste0(beginning, org_domain, ending)) %>%
    tidytable::select.(org_domain, org_name) %>% tibble::deframe()

  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)

  # drop missing emails, all domains to lower, extract domain info after @ sign
  all_domains_df <- df %>%
    as_tidytable() %>%
    tidytable::drop_na.(!!input) %>%
    tidytable::mutate.("{{ input }}" := tolower(!!input)) %>%
    tidytable::mutate.(domain = sub('.*@', '', !!input))

  # matches all of the root domains based on dictionary
  emails_df <- all_domains_df %>%
    tidytable::filter.(domain %in% academic_vector)

  # remove all of the subdomains and match again on roots
  emails_df <- all_domains_df %>%
    tidytable::filter.(str_count(domain, "[.]") == 2) %>%
    tidytable::mutate.(domain = sub("^.*?\\.", '', domain)) %>%
    tidytable::filter.(domain %in% academic_vector) %>%
    tidytable::bind_rows.(emails_df)

  # add institution names based on the classified email information
  # this essentially uses str_replace_all() to recode all domains into institutions
  emails_df <- emails_df %>%
    tidytable::mutate.("{{ output }}" := stringr::str_replace_all(domain, academic_deframed)) %>%
    tidytable::select.(!!id, !!input, !!output)

  # first we remove all of the already classified emails
  # and then it classifies emails with .edu as misc. academic
  # omitting ucar.edu bc it is a nonprofit
  # note: clearly room for improvement in non-us (.jp,.cn,.be,.br,.de,.fr,etc.)
  already_classified <- emails_df[,1] %>% flatten_chr()
  misc_df <- all_domains_df %>%
    tidytable::filter.(!!id %notin% already_classified &
                         grepl("\\.edu|\\.ac|uni-|univ-|alumni|alumno", domain) &
                         !grepl("ucar.edu", domain)) %>%
    tidytable::mutate.("{{ output }}" := "misc. academic") %>%
    tidytable::select.(!!id, !!input, !!output)

  # binds all the data back together and gives you the final df
  df <- misc_df %>%
    tidytable::bind_rows.(emails_df)
  df

}

# academic_clean_names()
academic_clean_names <- function(df, input, output){

  suppressMessages(library("dplyr"))
  suppressMessages(library("readr"))
  suppressMessages(library("tidyr"))
  suppressMessages(library("tidytable"))

  setwd("~/git/oss-2020/docs/")
  academic_dictionary <- readr::read_csv("academic_institutions.csv", col_types = cols()) %>%
    tidytable::mutate.(beginning = "\\b(?i)(", ending = ")\\b",
                       original_string = paste0(beginning, original_string, ending)) %>%
    tidytable::select.(original_string, new_string) %>% tibble::deframe()

  df <- df %>%
    tidytable::mutate.("{{output}}" := tolower({{ input }})) %>%
    tidytable::mutate.("{{output}}" := stringr::str_replace_all({{ output }}, academic_dictionary))
  df

}

# academic_detect_orgs()
academic_detect_orgs <- function(df, id, input, output, email){ # , email=FALSE, add sector too

  suppressMessages(library("dplyr"))
  suppressMessages(library("readr"))
  suppressMessages(library("tidyr"))
  suppressMessages(library("stringr"))
  suppressMessages(library("tidytext"))
  suppressMessages(library("tidytable"))

  seven_terms <- academic_dict_terms(5)
  six_terms <- academic_dict_terms(5)
  five_terms <- academic_dict_terms(5)
  four_terms <- academic_dict_terms(4)
  three_terms <- academic_dict_terms(3)
  two_terms <- academic_dict_terms(2)
  single_terms <- academic_dict_terms(1)

  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  email <- enquo(email)
  `%notin%` <- Negate(`%in%`)

  # clean up common misspellings and inconsistencies
  # may need to do move cleaning here (add daniel's functions here???)
  df_to_parse <- df %>%
    as_tidytable() %>%
    tidytable::drop_na.(!!input) %>%
    tidytable::mutate.(academic = tolower(!!input),
                       academic = stringr::str_replace(academic, "univ\\.", "university"),
                       academic = stringr::str_replace(academic, "univesity", "university"),
                       academic = stringr::str_replace(academic, "univeristy", "university"),
                       academic = stringr::str_replace(academic, "universoty", "university"),
                       academic = stringr::str_replace(academic, "a & m", "a&m"))

  # now we are going to catch all of the academic institutions using a funneling approach
  # this means we match all the five-grams, four-grams, three-grams, etc in our dictionary,
  # which preserves satellite campuses without requiring too much regex or computational time
  # after each step, we remove all of the classified rows to speed up the process

  seven_terms_df <- df_to_parse %>%
    funnel_ngrams({{id}}, {{input}}, academic, 7, seven_terms)
  already_classified <- seven_terms_df[,1] %>% flatten_chr()

  six_terms_df <- df_to_parse %>%
    funnel_ngrams({{id}}, {{input}}, academic, 6, six_terms)
  newly_classified <- six_terms_df[,1] %>% flatten_chr()
  already_classified <- c(already_classified, newly_classified)

  five_terms_df <- df_to_parse %>%
    funnel_ngrams({{id}}, {{input}}, academic, 5, five_terms)
  newly_classified <- five_terms_df[,1] %>% flatten_chr()
  already_classified <- c(already_classified, newly_classified)

  four_terms_df <- df_to_parse %>%
    tidytable::filter.(!!id %notin% already_classified) %>%
    funnel_ngrams({{id}}, {{input}}, academic, 4, four_terms)
  newly_classified <- four_terms_df[,1] %>% flatten_chr()
  already_classified <- c(already_classified, newly_classified)

  three_terms_df <- df_to_parse %>%
    tidytable::filter.(!!id %notin% already_classified) %>%
    funnel_ngrams({{id}}, {{input}}, academic, 3, three_terms)
  newly_classified <- three_terms_df[,1] %>% flatten_chr()
  already_classified <- c(already_classified, newly_classified)

  two_terms_df <- df_to_parse %>%
    tidytable::filter.(!!id %notin% already_classified) %>%
    funnel_ngrams({{id}}, {{input}}, academic, 2, two_terms)
  newly_classified <- two_terms_df[,1] %>% flatten_chr()
  already_classified <- c(already_classified, newly_classified)

  single_terms_df <- df_to_parse %>%
    tidytable::filter.(!!id %notin% already_classified) %>%
    tidytext::unnest_tokens(words, !!input) %>%
    tidytable::filter.(words %in% single_terms) %>%
    tidytable::mutate.(academic := 1) %>%
    tidytable::select.(!!id, words, academic)
  newly_classified <- single_terms_df[,1] %>% flatten_chr()
  already_classified <- c(already_classified, newly_classified)

  # after the funneling is completed, we classify the rest by emails
  # we opted to prioritize self-reported data over email domains
  emails_df <- df %>%
    as_tidytable() %>%
    tidytable::drop_na.(!!email) %>%
    tidytable::filter.(!!id %notin% already_classified) %>%
    academic_email_to_org(!!id, !!email, words) %>%
    tidytable::drop_na.(words) %>%
    tidytable::select.(!!id, words) %>%
    tidytable::mutate.(academic = 1)
  newly_classified <- emails_df[,1] %>% flatten_chr()
  already_classified <- c(already_classified, newly_classified)

  misc_academic_df <- df_to_parse %>%
    tidytable::filter.(!!id %notin% already_classified) %>%
    tidytext::unnest_tokens(tmp_words, !!input) %>%
    tidytable::filter.(tmp_words %in% c("student", "academic", "academia", "college",
                                        "bootcamp", "teacher", "professor",
                                        "university", "universidad", "universitat")) %>%
    tidytable::mutate.(words = "misc. academic", academic = 1) %>%
    tidytable::select.(!!id, words, academic)

  # then we combine all of the funneled data to the email data
  combined_df <- tidytable::bind_rows.(five_terms_df, four_terms_df,
                                       three_terms_df, two_terms_df,
                                       single_terms_df, emails_df,
                                       misc_academic_df ) %>%
    tidytable::distinct.(!!id, words, academic) %>%
    tidytable::rename.("{{output}}" := words)

  # standardize all of the academic institutions
  combined_df <- combined_df %>%
    academic_clean_names(!!output, !!output)

  # join to the original dataframe
  df <- df %>%
    tidytable::left_join.(combined_df) %>%
    tidytable::mutate.(academic := replace_na.(academic, 0)) %>%
    as.data.frame()

  df

  # second last step is to match on email bc we prioritize input column
  # any mention of academic terms outside of
  # still need to figure out why cornell/mit and others are duplicating (n=46)
     # this issue is before emails are added (emails add no dupes as of 7/24 so its a regex issue)

}

# academic_org_to_country()
academic_org_to_stats <- function(df, input, output){

  suppressMessages(library("dplyr"))
  suppressMessages(library("readr"))
  suppressMessages(library("tidyr"))
  suppressMessages(library("tidytable"))

  # ADD IN ANOTHER PARAMETER FOR COUNTRY NAME, IS0-2 OR IS0-3 OUT
  # private or public institution information, etc

  setwd("~/git/oss-2020/docs/")
  academic_to_country_dict <- readr::read_csv("academic_institutions.csv", col_types = cols()) %>%
    tidytable::mutate.(beginning = "\\b(?i)(", ending = ")\\b",
                       original_string = paste0(beginning, original_string, ending)) %>%
    tidytable::select.(new_string, country) %>% tibble::deframe()

  df <- df %>%
    tidytable::mutate.("{{output}}" := tolower({{ input }})) %>%
    tidytable::mutate.("{{output}}" := stringr::str_replace_all({{ output }}, academic_to_country_dict))
  df

}
