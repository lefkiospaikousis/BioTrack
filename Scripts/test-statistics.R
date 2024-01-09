

devtools::load_all()

library(DBI)

dbase_specimen <- DBI::dbConnect(RSQLite::SQLite(), "DB/prod/specimen")

samples <- dbase_specimen |>  
  tbl("sample_info") |> 
  collect() 

n_distinct(samples$bococ)

samples |> distinct(bococ)
samples |> distinct(bococ, phase)
samples |> distinct(phase)

diagnosis_types <- c("Colon", "Lung", "Breast")
rep_phase <- "Baseline"

tbl_counts <- samples |> 
  filter(diagnosis %in% diagnosis_types & phase == rep_phase) |> 
  select(bococ, diagnosis, date_collection) |> 
  distinct(bococ, .keep_all = TRUE) |> 
  mutate(
    date_collection = purrr::map(date_collection, to_date_time)
  ) |> 
  tidyr::unnest_longer(c(date_collection)) |> 
  mutate(date_collection = as.Date(date_collection),
         month = lubridate::month(date_collection) |> stringr::str_pad(2, pad = "0"),
         year = lubridate::isoyear(date_collection),
         isoweek = lubridate::isoweek(date_collection),
         year_month = paste0(year,month)
         ) |> 
  count(year_month, isoweek, diagnosis) |> 
  #mutate(diagnosis = toupper(diagnosis)) |> 
  group_by(year_month, diagnosis) |> 
  mutate(week = row_number() )|> 
  select(-isoweek) |> 
  tidyr::complete(week = c(1:5), fill = list( n = 0) ) |> 
  ungroup()
  
sum(tbl_counts$n)


tbl_counts |> filter(diagnosis == "Colon")


ttl_week <- tbl_counts |> 
  summarise(n = sum(n), .by = c(year_month, diagnosis)) |> 
  mutate(week = "Total")

ttl_diagnosis <- tbl_counts |> 
  summarise(n = sum(n), .by = c(year_month, week)) |> 
  mutate(diagnosis = "Total") |> 
  mutate(week = as.character(week))

tbl_totals <- tbl_counts |> 
  mutate(week = as.character(week)) |> 
  bind_rows(ttl_week) |> 
  bind_rows(ttl_diagnosis) |> 
  arrange(year_month, diagnosis, week) |> 
  tidyr::pivot_wider(names_from = diagnosis, values_from = n, values_fill = 0) |> 
  relocate(Total, .after = last_col()) |> 
  mutate(
    year_month = lubridate::ym(year_month) |> format("%b-%Y")
  ) |> 
  rename(
    Month = year_month,
    Week = week
  )

rep_header <- glue::glue("Recruited patients at {rep_phase}")

tbl_totals |> 
  flextable::flextable() |> 
  flextable::theme_box() |> 
  flextable::merge_v(j = "Month") |> 
  flextable::add_header_row(values = c("", rep_header), colwidths = c(2, ncol(tbl_totals) - 2))
  


library(flextable)

ft01 <- fp_text_default(color = "red")
ft02 <- fp_text_default(color = "orange")

pars <- as_paragraph(
  as_chunk(c("(1)", "(2)"), props = ft02), " ",
  as_chunk(c("My tailor is rich",
             "My baker is rich"), props = ft01)
)

ft_1 <- flextable(head(mtcars))
ft_1 <- add_header_row(ft_1, values = pars,
                       colwidths = c(5, 6), top = FALSE)
ft_1 <- add_header_row(ft_1, values = pars,
                       colwidths = c(3, 8), top = TRUE)
ft_1

ft_2 <- flextable(head(airquality))
ft_2 <- add_header_row(ft_2, values = c("Measure", "Time"),
                       colwidths = c(4, 2), top = TRUE)
ft_2 <- theme_box(ft_2)
ft_2







