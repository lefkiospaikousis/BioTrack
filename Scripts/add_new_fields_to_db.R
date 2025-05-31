
library(dplyr)

load_database()

devtools::load_all()

path_dbase_specimen<- BioTrack:::get_golem_config("path_db_specimen")

dbase_specimen <- DBI::dbConnect(RSQLite::SQLite(), path_dbase_specimen)


dbase_specimen %>% 
  DBI::dbListTables()
# add new fields to the sample_info table
dbase_specimen %>% 
  DBI::dbExecute(
    "ALTER TABLE sample_info ADD COLUMN sample_origin TEXT"
  )

dbase_specimen %>% 
  DBI::dbExecute(
    "ALTER TABLE sample_info ADD COLUMN location_lesion TEXT"
  )

dbase_specimen %>% 
  DBI::dbExecute(
    "ALTER TABLE sample_info ADD COLUMN anatomical_site TEXT"
  )

dbase_specimen %>% 
  DBI::dbExecute(
    "ALTER TABLE sample_info ADD COLUMN sampling_technique TEXT"
  )

dbase_specimen %>% 
  DBI::dbExecute(
    "ALTER TABLE sample_info ADD COLUMN sampling_technique_other TEXT"
  )

# add new fields to the specimen_info table

dbase_specimen %>% 
  DBI::dbExecute(
    "ALTER TABLE specimen_info ADD COLUMN tumour_cellularity TEXT"
  )

dbase_specimen %>% 
  DBI::dbExecute(
    "ALTER TABLE specimen_info ADD COLUMN surface_area TEXT"
  )

dbase_specimen %>% 
  DBI::dbExecute(
    "ALTER TABLE specimen_info ADD COLUMN n_blocks INTEGER"
  )

dbase_specimen %>% 
  DBI::dbExecute(
    "ALTER TABLE specimen_info ADD COLUMN n_slides INTEGER"
  )

dbase_specimen %>%
  DBI::dbExecute(
    "ALTER TABLE specimen_info ADD COLUMN histopathology_id TEXT"
  )

close_database()
