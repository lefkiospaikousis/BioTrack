# Sometimes the sample information is lost
# while the specimen information are saved
# Because the specimen information are stored to DB as they are created
# While the Same Information is stored to DB when the user presses submit
# Sometimes, the users log out before the final sample information submission


devtools::load_all()

dbase_specimen <- DBI::dbConnect(RSQLite::SQLite(), "DB/prod/specimen")

lab_no <- "242246PL"

unique_id <- dbase_specimen |> 
  tbl("specimen_info") |> 
  filter(lab_no == !!lab_no) |> 
  pull(unique_id)

tbl_sample_info <- data.frame(
  unique_id = unique_id,
  bococ = "051672"
)

DBI::dbAppendTable(dbase_specimen, "sample_info", tbl_sample_info)

DBI::dbDisconnect(dbase_specimen)
