# Configuration, database and onStop

# RSQLite connection ------------------------------------------------------

path_dbase_specimen<- BioTrack:::get_golem_config("path_db_specimen")

dbase_specimen <- DBI::dbConnect(RSQLite::SQLite(), path_dbase_specimen)

# on Firefox the form's input are not reset with shinyjs::reset()
jsResetCode <- "shinyjs.reset1 = function() {history.go(0)}" # {window.location.reload(true)}"

shiny::onStop(function(){
  
  cat("Doing application cleanup\n")
  cat("-----\n Removing connections\n")
  
  DBI::dbDisconnect(dbase_specimen)
  
})
