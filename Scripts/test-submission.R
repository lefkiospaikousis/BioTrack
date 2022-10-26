


library(dplyr)
library(purrr)

submission <- readRDS("submission_empty_dates.rds")


submission


submission %>% 
  modify_if(lubridate::is.Date, function(x){
    
    if(length(x) == 0) NA_integer_
  })

x <- submission %>% 
  map_if(lubridate::is.timepoint, function(x){
    
    if(length(x) == 0) {
      NA_integer_
    } else {
      x
    }
  }) 

waldo::compare(submission, x)
as.data.frame(submission) %>% 
  glimpse()


load_database()


dbase_specimen %>% 
  tbl("sample_info") %>% 
  glimpse()


x <- dbase_specimen %>% 
  tbl("sample_info") %>% 
  collect() %>% 
  slice(4) 

to_date_time(x$date_receipt) %>% class

to_date(x$dob) %>% class


DBI::dbAppendTable(dbase_specimen, "sample_info", as.data.frame(submission))


x <- as.Date("2022-11-01")

as.integer(x)

y <- Sys.time()

y

as.integer(y)
