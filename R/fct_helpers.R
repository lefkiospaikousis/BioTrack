#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
clean_string <- function(string){
  
  gsub("[[:punct:]]", "_", string)
  
}