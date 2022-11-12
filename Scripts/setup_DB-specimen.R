

devtools::load_all()

library(dplyr)
library(dbplyr)
library(DBI)

# Forms DB ----


path_db <- "DB/prod/specimen"

dbase_specimen <- DBI::dbConnect(RSQLite::SQLite(), path_db)

dbListTables(dbase_specimen)

## Sample Info ----

dta_sample_info = dplyr::tibble(
  
  time_stamp        = 123L, # Time stamp of submission date
  unique_id         = uuid::UUIDgenerate(),
  firstname         = character(1),
  surname           = character(1),
  gender            = character(1),
  bococ             = character(1),
  dob               = 123L,
  nationality       = character(1),
  diagnosis         = character(1),
  status            = character(1),
  doctor            = character(1),
  consent           = character(1),
  
  type1             = character(1),
  type1_ml          = numeric(1),
  type2             = character(1),
  type2_ml          = numeric(1),
  type3             = character(1),
  type3_ml          = numeric(1),
  type4             = character(1),
  type4_ml          = numeric(1),
  type5             = character(1),
  type5_ml          = numeric(1),
  
  tube              = character(1),
  phase             = character(1),
  at_bococ          = character(1),
  date_collection   = 123L,
  date_shipment     = 123L,
  date_receipt      = 123L,
  civil_id          = character(1),
  study_id          = character(1),
  study             = character(1),
  comments          = character(1),
  
  specimens         = 1L, # how many specimens added?
  
  path_icf          = character(1)
)


dta_specimen_type = dplyr::tibble(
  
  time_stamp2       = 123L, # Time stamp of submission date
  year              = 2022L,
  serial            = 100L,
  unique_id         = uuid::UUIDgenerate(),
  date_processing   = 123L,
  duration          = character(1),
  quality           = character(1),
  specimen_type     = character(1),
  lab_no            = character(1),
  freezer           = character(1),
  rack              = character(1),
  drawer            = character(1),
  box               = character(1),
  place             = character(1),
  n_tubes           = 1L
  
)

## Save to DB ----

dbWriteTable(dbase_specimen, "sample_info", dta_sample_info, overwrite = TRUE)
dbWriteTable(dbase_specimen, "specimen_info", dta_specimen_type, overwrite = TRUE)


dbase_specimen %>% 
  tbl("specimen_info") %>% 
  glimpse()

close_database()


## Add Columns?

devtools::load_all()

load_database()

dbase_specimen %>% DBI::dbListTables()

sql_1 <- paste0("ALTER TABLE specimen_info ADD time_stamp2 INT")

rs <- DBI::dbExecute(dbase_specimen, sql_1)

dbase_specimen %>% DBI::dbListTables()

dbase_specimen %>%
  tbl("specimen_info") %>%
  glimpse()

close_database()
DBI::dbDisconnect(dbase_specimen)
