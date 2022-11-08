#' db_operations 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_fromDB <- function(conn, table, what){
  
  conn %>% 
    tbl(table) %>% 
    collect() %>% 
    pull(what) %>% 
    unique()
  
}

#get_fromDB(conn, "departments", "dep_name")


inform_who <- function(conn, incident_code){
  
  dta <- conn %>% 
    tbl("inform") %>% 
    collect()
  
  stopifnot(incident_code %in% dta$incident_code)
  
  dta %>% 
    tidyr::pivot_longer(-incident_code) %>% 
    filter(incident_code == !!incident_code) %>% 
    filter(value == 1) %>% 
    pull(name)
  
}

recipient_info <- function(conn, department, what = c("name", "email")){
  
  dta <- conn %>% 
    tbl("departments") %>% 
    collect()
  
  stopifnot(department %in% dta$dep_name)
  
  dta <- filter(dta, dep_name == department)
  
  name = dta[["dep_head"]]
  email <- setNames(dta[["dep_head_email"]], name)
  list(
    name = name,
    email = email
  )
  
}


get_from_table <- function(conn, table, what, id){
  
  conn %>% 
    tbl(table) %>% 
    filter(unique_id == !!id) %>% 
    pull(what)
  
}



get_full_table <- function(conn, table){
  
  conn %>% tbl(table) %>% 
    collect() %>% 
    process_incidents() %>% 
    select(unique_id, any_of(column_labels))
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
