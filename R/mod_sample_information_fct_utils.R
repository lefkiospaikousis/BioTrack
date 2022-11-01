# Validation rules for shinyvalidate

no_negative <- function(value){
  if (value < 0) "Negative values are not allowed"
}

at_least_nchar <- function(text, nchars){
  if(nchar(text) < nchars) {
    paste0("Τουλάχιστο ", nchars, " χαρακτήρες")
  }
}

valid_bococ <- function(value){
  if(nchar(value) > 6) "Not a valid BOCOC number (max 6 digits)"
}

human_time <- function(time = Sys.time()) {
  format(time, "%d/%m/%Y-%H:%M:%OS")
}

#' time stamp for files
#' @param time Posixct time
file_time <- function(time = Sys.time()) {
  format(time, "%Y%m%d_%H%M%S")
}

epochTime <- function(time = Sys.time()) {
  as.integer(time)
}

#' Get date out of numeric format time
#' 
#' Translates a number to a date. 
#' Considers origin the 01/01/1970
#' 
#' @param number A number that needs be translated to date
#' @param .tz Time zone
to_date_time <- function(number, .tz = "EET"){
  
  stopifnot(inherits(number, c("numeric", "integer")))
  
  lubridate::as_datetime(number, tz = .tz)
}

to_date <- function(number){
  
  stopifnot(inherits(number, c("numeric", "integer")))
  
  lubridate::as_date(number)
}

lapsed_time <- function (start_time, end_time = Sys.time()) 
{
  x <- lubridate::seconds_to_period(difftime(end_time, start_time, units = "secs"))
  as.character(round(x, 1))
}

process_submission <- function(submission){
  
  stopifnot(inherits(submission, "list"))
  
  submission$unique_id  <- uuid::UUIDgenerate()
  
  submission$time_stamp <- epochTime()  
  
  submission$bococ <- stringr::str_pad(submission$bococ, 6, 'left', '0')
  # # because the returned value is a date-time
  submission$time_receipt <- strftime(submission$time_receipt, "%R")
  submission$date_receipt <- ymd(submission$date_receipt, tz = "EET") + hm(submission$time_receipt)
  submission$time_receipt <- NULL
  
  # Initialise the number of specimens - 0 for now.Will be updated later in the mod_add_specimen
  submission$specimens <- 0
  
  # submission$time_processing <- strftime(submission$time_processing, "%R")
  # submission$date_processing <- ymd(submission$date_processing, tz = "EET") + hm(submission$time_processing)
  # submission["time_processing"] <- NULL
  
  # If Date(0) object or zero length dateTime object, then needs to be NA_integer_
  # otherwise cannot be save in the DB. lubridate::is.timepoint works for both Date and Posixct objects
  # So if any of the date or datetime inputs are not there (e.g. date_shipment is not mandatory) this 
  # will make sure all is good for saving in the DB
  submission <- submission %>% 
    purrr::map_if(lubridate::is.timepoint, function(x){
      
      if(length(x) == 0) {
        NA_integer_
      } else {
        x
      }
    }) 
  
  # submission$date_collection <- as.integer(submission$date_collection)
  # submission$date_shipment <- as.integer(submission$date_shipment)
  # submission$date_processing <- as.integer(submission$date_processing)
  # submission$date_receipt <- as.integer(submission$date_receipt)
  
  # if nulls turn them to NA_character. Used for the check boxes
  submission <- map(submission, ~ . %||% NA_character_)
  
  submission 
  
}



max_serial_year <- function(conn, year_now){
  
  curr_year <- conn %>% 
    tbl("specimen_info") %>% 
    filter(year == year_now) 
  
  n_rows <- count(curr_year) %>% collect() %>% .$n
  
  # When the year switches, we wont have any data to get the max serial
  if(n_rows == 0){
    
    return(0)
    
  } else {
    
    curr_year %>% 
      filter(serial == max(serial)) %>% 
      pull(serial) %>% .[1]
  }
  
  
}
