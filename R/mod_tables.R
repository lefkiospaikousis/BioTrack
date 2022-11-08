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
mod_tables_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    
    mod_table_registry_server("table_registry_1", tbl_merged)
    mod_table_storage_server("table_storage_1", tbl_merged)
    
    
    tbl_registry <- reactive({
      session$userData$db_trigger()
      dbase_specimen %>% 
        tbl("sample_info") %>% 
        collect() 
      
    })
    
    tbl_specimen <- reactive({
      session$userData$db_trigger()
      dbase_specimen %>% 
        tbl("specimen_info") %>% 
        collect()
      
    })
    
    
    tbl_merged <- reactive({
      #session$userData$db_trigger()
      merged <- left_join(tbl_registry(), tbl_specimen(), by = "unique_id") 
      
      merged %>% 
        mutate(
          date_receipt = to_date_time(date_receipt),
          date_collection = to_date(date_collection),
          dob = to_date(dob),
          date_shipment = to_date(date_shipment)
        )
    })
    
    
  })
}

## To be copied in the UI
# mod_tables_ui("tables_1")

## To be copied in the server
# mod_tables_server("tables_1")
