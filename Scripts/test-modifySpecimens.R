
devtools::load_all()


load_database()


dbase_specimen %>% DBI::dbListTables()


# specimen_lab_no <- "2200146PL"
# 
# specimen <- dbase_specimen %>% 
#   tbl("specimen_info") %>% 
#   filter(lab_no == !!specimen_lab_no) %>% 
#   collect() %>% 
#   as.list()
# 
# 
# tagList(
#   p("Specimen Information"),
#   p("Lab no", specimen$lab_no),
#   p("Quality of Sample", specimen$quality),
#   p("Specimen Type", specimen$specimen_type),
#   p("Freezer", specimen$freezer , "Position: ", specimen$place),
#   p("Date processing", to_date_time(specimen$date_processing)),
#   p("Duration from Receipt to Processing", specimen$duration),
#   p("Number of tubes", specimen$n_tubes),
# )  



library(gt)
library(tidyr)
library(flextable)

storage <- dbase_specimen %>% 
  tbl("specimen_info") %>% 
  collect()

sample <- dbase_specimen %>% 
  tbl("sample_info") %>% 
  collect()


freezer <- "-80"
title <- glue::glue("{freezer}°C Freezer A Specimen Log")
log_version <- "Version 1.0 July 2021"

page_props <- officer::prop_section(page_size = officer::page_size(orient = "landscape"))
  
nest <- storage %>% 
  left_join(sample[, c("unique_id", "firstname", "surname", "date_collection")], by = "unique_id") %>% 
  mutate(initials =  paste0(substr(firstname, 1, 1), substr(surname, 1, 1)),
         date_collection = to_date(date_collection) %>% format("%d/%m/%Y")
         ) %>% 
  unite(content, c(lab_no, initials, date_collection), remove = TRUE,sep = "-") %>% 
  filter(freezer == !!freezer) %>% 
  select(rack, box, content) %>% 
  filter(!is.na(box)) %>% 
  complete(rack = LETTERS[1:4], box = as.character(c(1:5))) %>% 
  rename (Rack = rack)

nest %>% 
  #tibble::rowid_to_column() %>% 
  pivot_wider( names_from = box, 
               values_from = content, 
               #values_fn = list,
               values_fn = ~glue::glue_collapse(., sep = "\n")
               ) %>% 
  # tidyr::unnest(cols = c(`1`, `2`, `3`, `4`, `5`)) %>% 
  flextable::flextable() %>% 
  add_header_row(values = c("", "Box"), colwidths = c(1, 5)) %>% 
  theme_box() %>%
  set_caption(title) %>% 
  {.} %>% 
  set_table_properties("fixed") %>% 
  width(width = 1.80) %>% 
  fontsize(size = 9) %>% 
  add_footer_lines(glue::glue("Date exported: ", format(Sys.time(), "%d/%m/%Y %H:%M"))) %>% 
  add_footer_lines(glue::glue("BOCOC {freezer} Freezer. Specimen Log {log_version}"))
  # dim()
  flextable::save_as_docx(
    path = "test2.docx",
    pr_section = page_props
    )
  writexl::write_xlsx("test1.xlsx")


close_database()
