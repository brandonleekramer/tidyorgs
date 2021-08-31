
check_if_df <- function(data){
  if(data.class(df) != "data.frame") stop("Object must be a data frame.")
}

id_var_error <- function(id_test){
  if(!rlang::is_character(id_test)) stop("The id variable must be character vector.") 
}

input_var_error <- function(input_test){
  if(!rlang::is_character(input_test)) stop("The input variable must be character vector.") 
}

email_var_error <- function(email_test){
  if(!rlang::is_character(email_test)) stop("The email variable must be character vector.") 
}

check_if_char_var <- function(id, input, email){
  if(missing(email)){
    id_var_error(id)
    input_var_error(input)
  } else {
    id_var_error(id)
    input_var_error(input)
    email_var_error(email)
  }
}

#check_if_char_var(df, "id", "input")




