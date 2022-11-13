#' log_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_log_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Log file"),
    box(width = 10,
      mod_downloadTable_ui(ns("down_log")),
      reactable::reactableOutput(ns("log_file"))
    )
  )
}

#' log_file Server Functions
#'
#' @noRd 
mod_log_file_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    mod_downloadTable_server("down_log", "LOG FILE", log_file)
    
    log_file <- reactive({
      
      session$userData$db_trigger
      
      dbase_specimen %>% 
        tbl("log_file") %>% 
        collect()
    })
    
    output$log_file <- renderReactable({
      
      session$userData$db_trigger
      
      log_file() %>% 
        reactable(
          searchable = TRUE,
          columns = list(
            time_stamp3 = colDef(name = 'Time Stamp'),
            bococ = colDef(name = col_labels[["bococ"]]),
            lab_no = colDef(name = col_labels[["lab_no"]]),
            action = colDef(name = "Action by the user"),
            comments = colDef("Comments")
          )
        )
    })
    
  })
}

## To be copied in the UI
# mod_log_file_ui("log_file_1")

## To be copied in the server
# mod_log_file_server("log_file_1")
