#' The lab got a new freezer, so I need to migrate the data from the old freezer to the new one.
#' The old freezer is a 20L freezer with drawers, and the new one is still a 20L freezer but with Racks..
#' 
#' 


# Load the data

devtools::load_all()
path_dbase_specimen<- 'DB/prod/specimen'

dbase_specimen <- DBI::dbConnect(RSQLite::SQLite(), path_dbase_specimen)

dbase_specimen |> DBI::dbListTables()


all_database <- dbase_specimen |> tbl("specimen_info") |> collect()


freezer_20 <- all_database |> 
  filter(freezer == "-20°C") 


new_info <- freezer_20 |> 
  mutate(rack = case_when(
    drawer == "1" ~ "A1",
    drawer == "3" ~ "B2",
    drawer == "4" ~ "B2"
  )) |> 
  # empty the drawer
  mutate(drawer = '') |> 
  # recalculate the position
  mutate(
    place = glue::glue('{rack}.{drawer}.{box}')
  ) 

new_info |> count(rack)
new_info |> count(drawer)
new_info |> count(box)
new_info |> count(place)

# Update a copy of the database to check before updating in Place
new_db <- dbase_specimen |> 
  tbl("specimen_info") |> 
  rows_update(new_info, 
              by = "lab_no", 
              copy = TRUE, 
              unmatched = c("ignore")
  ) |> 
  collect() 


new_db|> 
  filter(freezer == "-20°C") |>
  count(place)

# did we update the database correctly?
# not the best way to check, but it is a way to check

# other than the freezer_20
all.equal(
  new_db|> filter(freezer != "-20°C"),
  all_database |> filter(freezer != "-20°C") 
)

# the freezer_20 should have mismatches in the rack, drawer and place only
nrow(new_db)
nrow(all_database)

new_20 <- new_db |> filter(freezer == "-20°C")
old_20 <- all_database |> filter(freezer == "-20°C")

nrow(new_20) == nrow(old_20)

all.equal(new_20, old_20)

setdiff(old_20, new_20) |> count(drawer)
setdiff(new_20, old_20) |> count(rack)

# ALL ARE GOOD then update the prod DB `specimen` in place
# there is a copy already in the prod folder `specimen - Copy` that has the old information

# UPDATE in PLACE
dbase_specimen |> 
  tbl("specimen_info") |> 
  rows_update(new_info, 
              by = "lab_no", 
              copy = TRUE, 
              unmatched = c("ignore"),
              in_place = TRUE
  ) 


# close the database to see the changes

close_database()

