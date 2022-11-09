
devtools::load_all()

load_database()


dbase_specimen %>% DBI::dbListTables()


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
  left_join(
    sample[, c("unique_id", "firstname", "surname", "date_collection")], 
    by = "unique_id"
  ) %>% 
  mutate(
    initials =  paste0(substr(firstname, 1, 1), substr(surname, 1, 1)),
    date_collection = to_date(date_collection) %>% format("%d/%m/%Y")
  ) %>% 
  unite(content, c(lab_no, initials, date_collection), remove = TRUE,sep = "-") %>% 
  filter(freezer == !!freezer) %>% 
  select(rack, box, content) %>% 
  filter(!is.na(box)) %>% 
  complete(rack = LETTERS[1:4], box = as.character(c(1:5))) %>% 
  rename (Rack = rack)

nest %>% 
  pivot_wider( 
    names_from = box, 
    values_from = content, 
    values_fn = ~glue::glue_collapse(., sep = "\n")
  ) %>% 
  flextable::flextable() %>% 
  add_header_row(values = c("", "Box"), colwidths = c(1, 5)) %>% 
  theme_box() %>%
  set_caption(title) %>% 
  set_table_properties("fixed") %>% 
  width(width = 1.80) %>% 
  fontsize(size = 9) %>% 
  add_footer_lines(glue::glue("Date exported: ", format(Sys.time(), "%d/%m/%Y %H:%M"))) %>% 
  add_footer_lines(glue::glue("BOCOC: {freezer} Freezer. Specimen Log {log_version}")) %>% 
  # dim()
  flextable::save_as_docx(
    path = "test2.docx",
    pr_section = page_props
  )


close_database()
