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
                                                   comments = glue::glue("Modified data for BOCOC: `{info$bococ}`: 
                                                                         Changed `{col_labels[[info$col]]}` to `{info$new_value}` 
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
