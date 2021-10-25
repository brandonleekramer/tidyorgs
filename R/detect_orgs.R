

detect_orgs <- function(data, id, input, output, 
                        email = FALSE,
                        country = FALSE, 
                        parent_org = FALSE, 
                        org_type = FALSE){
  
  id <- dplyr::enquo(id)
  input <- dplyr::enquo(input)
  output <- dplyr::enquo(output)
  email <- dplyr::enquo(email)
  
  if (missing(email)) {
    
    classified_academic <- data %>%
      detect_academic(!!id, !!input, !!output) 
    classified_businesses <- data %>%
      detect_business(!!id, !!input, !!output) 
    classified_goverment <- data %>%
      detect_government(!!id, !!input, !!output) 
    classified_nonprofit <- data %>%
      detect_nonprofit(!!id, !!input, !!output) 
    classified_orgs <- bind_rows(classified_academic, classified_businesses,
                                 classified_goverment, classified_nonprofit) 
    
  } else {
    
    classified_academic <- data %>%
      detect_academic(!!id, !!input, !!output, !!email) 
    classified_businesses <- data %>%
      detect_business(!!id, !!input, !!output, !!email) 
    classified_goverment <- data %>%
      detect_government(!!id, !!input, !!output, !!email) 
    classified_nonprofit <- data %>%
      detect_nonprofit(!!id, !!input, !!output, !!email) 
    classified_orgs <- bind_rows(classified_academic, classified_businesses,
                                 classified_goverment, classified_nonprofit) 
  
  } 
  
}