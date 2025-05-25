
#' UI for the Storage Place
#' 
#' This is a function that returns the UI for the Storage Place of a specimen
#' It is used in both the adding a specimen for the first time, and also for modifying the
#' specimen
storage_placeUI <- function(ns){
  
  tagList(
    
    tags$tr(width = "100%",
            tags$td(width = "30%", h4("Storage place"))),
    
    tags$tr(width = "100%",
            tags$td(width = "30%", div(class = "input-label",style = "", "Temperature:")),
            tags$td(width = "70%", prettyRadioButtons(ns("freezer"), NULL, col_values[["freezer"]], 
                                                      inline = TRUE,  fill = TRUE, selected = character(0), width = "100%"))),
    
    tags$tr(width = "100%",
            tags$td(width = "30%", div(class = "input-label", "Rack:")),
            tags$td(width = "70%", shinyjs::disabled(
              selectInput(ns("rack"), NULL, NULL, selected = character(0), width = "35%"))
            )),
    
    tags$tr(width = "100%",
            tags$td(width = "30%", div(class = "input-label", "Drawer:")),
            
            tags$td(width = "70%",  shinyjs::disabled(
              prettyRadioButtons(ns("drawer"), NULL, NULL,
                                 inline = TRUE,  fill = TRUE, selected = character(0), width = "100%"))
            )),
    
    tags$tr(width = "100%",
            tags$td(width = "30%", div(class = "input-label", "Box:")),
            tags$td(width = "70%",  shinyjs::disabled(
              prettyRadioButtons(ns("box"), NULL, NULL,
                                 inline = TRUE,  fill = TRUE, selected = character(0), width = "80%"))
            ))
  )
  
}