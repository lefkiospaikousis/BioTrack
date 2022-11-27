#' table_registry UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_registry_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_downloadTable_ui(ns("down_registry"), "Download Registry LOG as .xlsx"),
    div(mod_download_icf_ui(ns("download_icf"))),
    reactableOutput(ns("tbl_registry"))
    
  )
}

#' table_registry Server Functions
#'
#' @noRd 
mod_table_registry_server <- function(id, merged){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mod_downloadTable_server("down_registry", "SPECIMEN REGISTRY LOG", tbl_registry_down)
    
    mod_download_icf_server("download_icf", lab_no_to_download)
    
    selected <- reactive({
      
      reactable::getReactableState("tbl_registry", "selected")
      
    })
    
    lab_no_to_download <- eventReactive(selected(), {
      
      req(selected())
      
      lab_nos <- tbl_registry()[selected(), ]$lab_no
      
      merged() %>% 
        filter(lab_no %in% lab_nos) %>% 
        distinct(lab_no, path_icf, unique_id)
      
      
    })
    
    
    
    tbl_registry <- reactive({
      
      merged() %>% 
        select(
          lab_no, bococ, date_receipt, date_collection, 
          surname, firstname, civil_id, tube, specimen_type, 
          doctor, diagnosis, study, study_id, comments
        ) 
      
    })
    
    # rename for download
    tbl_registry_down <- reactive({
      
      x <- col_labels[names(tbl_registry())]
      x <- names(x) %>% setNames(x) 
      
      rename(tbl_registry(), !!!x)
    })
    
    
    output$tbl_registry <- renderReactable({
      
      tbl_registry() %>% 
        reactable(
          searchable = TRUE, highlight = TRUE, selection = "single", onClick = "select",
          pageSizeOptions = c(10, 25, 50, 100),
          columns = list(
            lab_no = colDef(name = col_labels[["lab_no"]]),
            bococ = colDef(name = col_labels[["bococ"]]),
            date_receipt = colDef( name = col_labels[["date_receipt"]], format = colFormat(datetime = TRUE, locales = "en-GB") ),
            date_collection = colDef( name = col_labels[["date_collection"]], format = colFormat(date = TRUE, locales = "el-GR") ),
            surname = colDef(name = col_labels[["surname"]]),
            firstname = colDef(name = col_labels[["firstname"]]),
            civil_id = colDef(name = col_labels[["civil_id"]]),
            tube = colDef(name = col_labels[["tube"]]),
            specimen_type = colDef(name = col_labels[["specimen_type"]]),
            doctor = colDef(name = col_labels[["doctor"]]),
            diagnosis = colDef(name = col_labels[["diagnosis"]]),
            study = colDef(name = col_labels[["study"]]),
            study_id = colDef(name = col_labels[["study_id"]]),
            comments = colDef(name = col_labels[["comments"]])
            
          )
          
        ) 
      
      
    })
    
  })
}

## To be copied in the UI
# mod_table_registry_ui("table_registry_1")

## To be copied in the server
# mod_table_registry_server("table_registry_1")
