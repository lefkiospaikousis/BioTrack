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
    span(mod_downloadTable_ui(ns("down_registry"), "Download Full Registry LOG as an .xlsx file"), 
         HTML("&nbsp;&nbsp;&nbsp;"), "OR", HTML("&nbsp;&nbsp;&nbsp;"),
         mod_download_icf_ui(ns("download_icf"))),
    hr(),
    fluidRow(
      col_2(
        shinyWidgets::pickerInput(ns("doctor"), col_labels[["doctor"]], choices = NULL, multiple = TRUE,
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
        shinyWidgets::pickerInput(ns("study"), col_labels[["study"]], choices = NULL, multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(
                                    liveSearchStyle = "contains",
                                    noneSelectedText = "ALL"
                                  )
        )
      )
    ),
    reactableOutput(ns("tbl_registry")) |> shinycssloaders::withSpinner()
    
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
    
    observeEvent(merged(), {
      
      doctors <- unique(merged()$doctor)
      shinyWidgets::updatePickerInput(session, "doctor", choices = c(doctors))
      
      statuses <- unique(merged()$status)
      shinyWidgets::updatePickerInput(session, "status", choices = c(statuses))
      
      studies <- unique(merged()$study)
      shinyWidgets::updatePickerInput(session, "study", choices = c(studies))
      
    })
    
    
    tbl_registry <- reactive({
      
      orgl <- merged() %>% 
        select(
          lab_no, bococ, civil_id, date_receipt, date_collection, type1, type1_ml, at_bococ, phase, 
          surname, firstname, gender, nationality, consent, dob,  tube, specimen_type,
          sample_origin, anatomical_site, location_lesion, sampling_technique, histopathology_id,
          doctor, diagnosis, status, study, study_id, path_icf, comments
        ) 
      
      out <- orgl
      
      if(!is.null(input$doctor)){
        out <- orgl %>% filter(doctor %in% input$doctor)
      }
      
      if(!is.null(input$status)){
        out <- out %>% filter(status %in% input$status)
      }
      
      if(!is.null(input$study)){
        out <- out %>% filter(study %in% input$study)
      }
      
      out
    })
    
    # rename for download
    tbl_registry_down <- reactive({
      
      x <- col_labels[names(tbl_registry())]
      x <- names(x) %>% setNames(x) 
      
      rename(tbl_registry(), !!!x)
    })
    
    
    output$tbl_registry <- renderReactable({
      
      tbl_registry() %>% 
        tidyr::unite("Patient name", surname, firstname, sep = " ", na.rm = TRUE) %>% 
        reactable(
          searchable = TRUE, highlight = TRUE, selection = "single", onClick = "select",
          pageSizeOptions = c(10, 25, 50, 100),
          columns = list(
            lab_no = colDef(name = col_labels[["lab_no"]]),
            bococ = colDef(name = col_labels[["bococ"]]),
            civil_id = colDef(name = col_labels[["civil_id"]]),
            date_receipt = colDef( name = col_labels[["date_receipt"]], format = colFormat(datetime = TRUE, locales = "en-GB") ),
            date_collection = colDef( name = col_labels[["date_collection"]], format = colFormat(datetime = TRUE, locales = "el-GR") ),
            type1 = colDef(name = col_labels[["type1"]]),
            type1_ml = colDef(name = col_labels[["type1_ml"]]),
            at_bococ = colDef( name = col_labels[["at_bococ"]]),
            phase = colDef( name = col_labels[["phase"]]),
            #surname = colDef(name = col_labels[["surname"]]),
            #firstname = colDef(name = col_labels[["firstname"]]),
            gender = colDef(name = col_labels[["gender"]]),
            nationality = colDef(name = col_labels[["nationality"]]),
            consent = colDef(name = col_labels[["consent"]]),
            dob = colDef(show = TRUE, name = col_labels[["dob"]], format = colFormat(date = TRUE, locales = "el-GR")),
            tube = colDef(name = col_labels[["tube"]]),
            specimen_type = colDef(name = col_labels[["specimen_type"]]),
            sample_origin = colDef(name = col_labels[["sample_origin"]]),
            anatomical_site = colDef(name = col_labels[["anatomical_site"]]),
            location_lesion = colDef(name = col_labels[["location_lesion"]]),
            sampling_technique = colDef(name = col_labels[["sampling_technique"]]),
            histopathology_id = colDef(name = col_labels[["histopathology_id"]]),
            doctor = colDef(name = col_labels[["doctor"]]),
            diagnosis = colDef(name = col_labels[["diagnosis"]]),
            status = colDef(name = col_labels[["status"]]),
            study = colDef(name = col_labels[["study"]]),
            study_id = colDef(name = col_labels[["study_id"]]),
            path_icf = colDef(name = col_labels[["path_icf"]]),
            comments = colDef(name = col_labels[["comments"]])
            
          ),
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#acb3c2", boxShadow = "inset 2px 0 0 0 #2b64e0")
          )
          
        ) 
      
      
    })
    
  })
}

## To be copied in the UI
# mod_table_registry_ui("table_registry_1")

## To be copied in the server
# mod_table_registry_server("table_registry_1")
