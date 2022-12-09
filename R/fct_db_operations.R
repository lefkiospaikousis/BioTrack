#' db_operations 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_fromDB2 <- function(conn, table, what){
  
  conn %>% 
    tbl(table) %>% 
    collect() %>% 
    pull(what) %>% 
    unique()
  
}

get_fromDB <- function(conn, table, what, id){
  
  conn %>% 
    tbl(table) %>% 
    filter(unique_id == !!id) %>% 
    collect() %>% 
    pull(what) %>% 
    unique()
  
}

#' Extract a specimen from the DB
#' 
#' @param conn the connection to the DB
#' @param lab_no The lan number
#' @export
#' @noRd
get_specimen <- function(conn, lab_no){
  
  conn %>% 
    dplyr::tbl("specimen_info") %>% 
    dplyr::filter(lab_no == !!lab_no) %>% 
    dplyr::collect() %>% 
    as.list()
}

#' Extract a sample info from the DB
#' 
#' @param conn the connection to the DB
#' @param unique_id The unique_id
#' @export
#' @noRd
get_sample_info <- function(conn, unique_id){
  
  conn %>% 
    dplyr::tbl("sample_info") %>% 
    dplyr::filter(unique_id == !!unique_id) %>% 
    dplyr::collect() %>% 
    as.list()
}





# specimen <- list()
# 
# load_database()
# 
# specimen$date_processing <- to_date_time(1671836990) #1671836780 #to_date_time(1671836490)
# specimen$lab_no <- "220008PL"
# specimen$date_collection <- 1671750060 #to_date_time(1671750060)
# specimen$duration <- "ddd"
# specimen$bococ <- 1223445
# close_database()

#update_duration(dbase_specimen, specimen, "user")

#' Update the duration from sample collection to sample processing
#' 
#' Calculates and updates to the database
#' 
#' This function will usually be called when the user changes either the 
#' date of collection or the date of processing
#' @export
#' @param conn the connection to the DB
#' @param specimen the specimen list
update_duration <- function(conn, specimen, user){
  
  if(!inherits(specimen$date_processing, "POSIXct") ){
    specimen$date_processing <- to_date_time(specimen$date_processing)
  }
  
  if(!inherits (specimen$date_collection, "POSIXct") ){
    specimen$date_collection <- to_date_time(specimen$date_collection)
  }
  
  stopifnot(length(specimen$lab_no) == 1 )
  
  stopifnot(all(c("date_processing", "date_collection") %in% names(specimen)))
  
  stopifnot(lubridate::is.POSIXct(specimen$date_collection))
  stopifnot(lubridate::is.POSIXct(specimen$date_processing))
  
  old_duration <- specimen$duration
  
  new_duration <- lapsed_time( specimen$date_collection, specimen$date_processing )
  
  x <- glue::glue_sql("UPDATE specimen_info SET duration = {new_duration} WHERE lab_no = {specimen$lab_no}", .con = conn)
  # might have more than 1 lab no's in case I change the date_collection of a sample_info with more than 1 lab_nos
  #x <- glue::glue_sql("UPDATE specimen_info SET duration = {new_duration} WHERE lab_no IN ({lab_nos*})", lab_nos = specimen$lab_no, .con = conn)
  
  
  rs <- DBI::dbExecute(conn, x)
  
  if(rs == 1){ 
    
    cat("Updated duration for ", rs, " specimen with lab_no: ", specimen$lab_no, "\n")
    
  } else {
    
    cat("Failed to update the duration for specimen with lab_no: ", specimen$lab_no, "\n")
    
  }
  # Add to log
  try({add_to_logFile("Modified specimen info", user, 
                      info = list(lab_no = specimen$lab_no,
                                  bococ = specimen$bococ,
                                  col = "duration",
                                  old_value = old_duration,
                                  new_value = new_duration
                                  
                      ))}, silent = TRUE)
  
  
}



#' Add DB modifications to a log entry
#'
#' @param what Action taken
#' @param who Who did it. The user
#' @param info A list of the information to print on the log file
#'
#' @return Saves on the DB
#' @export
add_to_logFile <- function(what, who, info){
  
  time <- as.character(lubridate::as_datetime(Sys.time(), "EET"))
  
  entry <- switch (what,
                   "Added Sample Information Form" = list(time_stamp3 = time, 
                                                          user = who, 
                                                          action = what, 
                                                          bococ = info$bococ,
                                                          comments = glue::glue("{what}: BOCOC-{info$bococ}")
                                                          ),
                   
                   "Added Specimen Type" = list(time_stamp3 = time, 
                                                user = who, 
                                                action = what, 
                                                bococ = info$bococ %||% NA_character_, 
                                                lab_no = info$lab_no,
                                                comments = glue::glue("{what}: {info$lab_no}")
                                                ),
                   
                   "Modified specimen info" = list(time_stamp3 = time, 
                                                   user = who, 
                                                   action = what, 
                                                   bococ = info$bococ %||% NA_character_, 
                                                   lab_no = info$lab_no, 
                                                   comments = glue::glue("Updated specimen `{info$lab_no}`: 
                                                                         Changed `{col_labels[[info$col]]}` to `{info$new_value}` 
                                                                         from `{info$old_value}`")),
                   
                   "Modified Sample Information data" = list(time_stamp3 = time, 
                                                   user = who, 
                                                   action = what, 
                                                   bococ = info$bococ %||% NA_character_, 
                                                   lab_no = info$lab_no, 
                                                   comments = glue::glue(
                                                   "Modified data for BOCOC: `{info$bococ}`: Changed `{col_labels[[info$col]]}` to `{info$new_value}` 
                                                                         from `{info$old_value}`")),
                   
                   stop("Unknown action in add_to_logFile")
  )
  
  res_save <- DBI::dbAppendTable(dbase_specimen, "log_file", as.data.frame(list(entry)))
  
  cat("Registered ", res_save, " event: '", what, "' in the LOG\n")
  
}


load_database <- function(){
  
  path <- file.path(pkgload::pkg_path(), "inst", "global.R")
  
  run_dev_lines <- readLines(path)
  
  eval(parse(text = run_dev_lines), envir = globalenv())
}

close_database <- function(){
  
  cat("Doing application cleanup\n")
  
  cat("-----\n Removing connections \n")
  
  DBI::dbDisconnect(dbase_specimen)
}
