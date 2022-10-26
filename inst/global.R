# Configuration, database and onStop

# RSQLite connection ------------------------------------------------------

#configuration <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "default")

path_dbase_specimen<- BioTrack:::get_golem_config("path_db_specimen")

dbase_specimen <- DBI::dbConnect(RSQLite::SQLite(), path_dbase_specimen)

shiny::onStop(function(){
  
  cat("Doing application cleanup\n")
  cat("-----\n Removing connections\n")
  
  DBI::dbDisconnect(dbase_specimen)
  #DBI::dbDisconnect(dbase_forms)
})
