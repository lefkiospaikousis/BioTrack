#' table_storage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_storage_ui <- function(id){
  ns <- NS(id)
  tagList(
    span(mod_downloadTable_ui(ns("down_storage"), "Download Full Storage LOG as an .xlsx file"), 
         HTML("&nbsp;&nbsp;&nbsp;"), "OR", HTML("&nbsp;&nbsp;&nbsp;"),
         mod_download_icf_ui(ns("download_icf2"))),
    hr(),
    fluidRow(
      col_2(
        shinyWidgets::pickerInput(ns("specimen_type"), col_labels[["specimen_type"]], choices = NULL, multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(
                                    liveSearchStyle = "contains",
                                    noneSelectedText = "ALL"
                                  )
        )
      ),
      col_2(
        shinyWidgets::pickerInput(ns("status"), col_labels[["status"]], choices = NULL, multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(
                                    liveSearchStyle = "contains",
                                    noneSelectedText = "ALL"
                                  )
        )
      ),
      col_2(
        shinyWidgets::pickerInput(ns("quality"), col_labels[["quality"]], choices = NULL, multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(
                                    liveSearchStyle = "contains",
                                    noneSelectedText = "ALL"
                                  )
        )
      )
    ),
    reactableOutput(ns("tbl_storage")) |> shinycssloaders::withSpinner()
  )
}
    
#' table_storage Server Functions
#'
#' @noRd 
mod_table_storage_server <- function(id, merged){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    mod_downloadTable_server("down_storage", "SPECIMEN STORAGE LOG", tbl_storage_down)
    
    mod_download_icf_server("download_icf2", lab_no_to_download)
    
    selected <- reactive({
      
      reactable::getReactableState("tbl_storage", "selected")
      
    })
    
    lab_no_to_download <- eventReactive(selected(), {
      
      req(selected())
      
      lab_nos <- tbl_storage()[selected(), ]$lab_no
      
      merged() %>% 
        filter(lab_no %in% lab_nos) %>% 
        distinct(lab_no, path_icf, unique_id)
      
      
    })
    
    observeEvent(merged(), {
      req(merged())
      specimen_types <- unique(merged()$specimen_type)
      shinyWidgets::updatePickerInput(session, "specimen_type", choices = c(specimen_types))
      
      statuses <- unique(merged()$status)
      shinyWidgets::updatePickerInput(session, "status", choices = c(statuses))
      
      qualities <- unique(merged()$quality)
      shinyWidgets::updatePickerInput(session, "quality", choices = c(qualities))
      
    })
    
    
    
    tbl_storage <- reactive({
      
      orgl <- merged() %>% 
        select(
          lab_no, bococ, date_receipt, date_collection, date_processing,
          surname, firstname, civil_id, specimen_type, status,
          quality, duration,  
          tumour_cellularity, surface_area,
          freezer, place, comment_place, n_tubes, n_blocks, n_slides
        )
      
      out <- orgl
      
      if(!is.null(input$specimen_type)){
        out <- orgl %>% filter(specimen_type %in% input$specimen_type)
      }
      
      if(!is.null(input$status)){
        out <- out %>% filter(status %in% input$status)
      }
      
      if(!is.null(input$quality)){
        out <- out %>% filter(quality %in% input$quality)
      }
      
      out
      
    })
    
    # rename for download
    tbl_storage_down <- reactive({
      
      x <- col_labels[names(tbl_storage())]
      x <- names(x) %>% setNames(x) 
      
      rename(tbl_storage(), !!!x)
    })
    
    
    output$tbl_storage <- renderReactable({
      
      
      tbl_storage() %>% 
        tidyr::unite("Patient name", surname, firstname, sep = " ", na.rm = TRUE) %>% 
        reactable(
          searchable = TRUE, highlight = TRUE, selection = "single", onClick = "select",
          pageSizeOptions = c(10, 25, 50, 100),
          columns = list(
            lab_no = colDef(name = col_labels[["lab_no"]]),
            bococ = colDef(name = col_labels[["bococ"]]),
            date_receipt = colDef( name = col_labels[["date_receipt"]], format = colFormat(datetime = TRUE, locales = "en-GB") ),
            date_collection = colDef( name = col_labels[["date_collection"]], format = colFormat(datetime = TRUE, locales = "en-GB") ),
            date_processing = colDef( name = col_labels[["date_processing"]], format = colFormat(datetime = TRUE, locales = "en-GB") ),
            #surname = colDef(name = col_labels[["surname"]]),
            #firstname = colDef(name = col_labels[["firstname"]]),
            civil_id = colDef(name = col_labels[["civil_id"]]),
            specimen_type = colDef(name = col_labels[["specimen_type"]]),
            status = colDef(name = col_labels[["status"]]),
            quality = colDef(name = col_labels[["quality"]]),
            duration = colDef(name = col_labels[["duration"]]),
            freezer = colDef(name = col_labels[["freezer"]]),
            place = colDef(name = col_labels[["place"]]),
            tumour_cellularity = colDef(name = col_labels[["tumour_cellularity"]]),
            surface_area = colDef(name = col_labels[["surface_area"]]),
            comment_place = colDef(name = col_labels[["comment_place"]]),
            n_tubes = colDef(name = col_labels[["n_tubes"]])
            
          ),
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#acb3c2", boxShadow = "inset 2px 0 0 0 #2b64e0")
          )
        ) 
      
      
    })
    
  })
}
    
## To be copied in the UI
# mod_table_storage_ui("table_storage_1")
    
## To be copied in the server
# mod_table_storage_server("table_storage_1")
