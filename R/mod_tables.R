#' tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom reactable reactable reactableOutput renderReactable colDef colFormat
mod_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shinydashboard::tabBox(id  = "tables_tab", width = 12,
                           tabPanel("SPECIMEN REGISTRY LOG",
                                    mod_table_registry_ui(ns("table_registry_1"))
                           ),
                           tabPanel("SPECIMEN STORAGE LOG",
                                    mod_table_storage_ui(ns("table_storage_1"))
                           )
    )
  )
}

#' tables Server Functions
#'
#' @noRd 
mod_tables_server <- function(id, tbl_merged){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    mod_table_registry_server("table_registry_1", tbl_merged)
    mod_table_storage_server("table_storage_1", tbl_merged)
    
    
  })
}

## To be copied in the UI
# mod_tables_ui("tables_1")

## To be copied in the server
# mod_tables_server("tables_1")
