
devtools::load_all()


load_database()


dbase_specimen %>% DBI::dbListTables()


specimen_lab_no <- "2200136PL"

specimen <- dbase_specimen %>% 
  tbl("specimen_info") %>% 
  filter(lab_no == specimen_lab_no) %>% 
  collect() %>% 
  as.list()


tagList(
  p("Specimen Information"),
  p("Lab no", specimen$lab_no),
  p("Quality of Sample", specimen$quality),
  p("Specimen Type", specimen$specimen_type),
  p("Freezer", specimen$freezer , "Position: ", specimen$place),
  p("Date processing", to_date_time(specimen$date_processing)),
  p("Duration from Receipt to Processing", specimen$duration),
  p("Number of tubes", specimen$n_tubes),
)  



