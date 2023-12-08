#' view_edit_specimen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_view_edit_specimen_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      # To set he focus on the view tab for scanning the LAB NO
      tags$script("
      Shiny.addCustomMessageHandler('selectText', function(NULL) {
        $('#view_edit_specimen_1-lab_no').select();
      });
    ")
    ),
    fluidRow(
      col_4(textInput(ns("lab_no"), "Specimen Lab No"))
    ),
    fluidRow(
      col_4(
        uiOutput(ns("lab_noUI"))
      ),
      col_4(
        uiOutput(ns("sample_infoUI"))
      )
    )
  )
}

#' view_edit_specimen Server Functions
#'
#' @noRd 
mod_view_edit_specimen_server <- function(id, focus){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      specimen_selected = NULL,  # a dataframe of the specimen
      sample_selected = NULL     # a dataframe of the samplee_info
    )
    
    iv <- shinyvalidate::InputValidator$new()
    
    iv$add_rule("lab_no", sv_required())
    iv$add_rule("lab_no", function(value){
      
      if(!nchar(value) == 8){
        "A 8-character code is needed"
      }
    })
    
    observe({
      req(input$lab_no)
      if(nchar(input$lab_no) > 1 && nchar(input$lab_no) < 8 | nchar(input$lab_no) > 8){
        iv$enable()
      } else {
        iv$disable()
      }
    })
    
    observeEvent(focus(),{
      # To set he focus on the view tab for scanning the LAB NO
      session$sendCustomMessage("selectText", list(NULL))
      #} 
    }, ignoreInit = TRUE)
    
    
    observeEvent(input$lab_no, {
      
      req(input$lab_no, cancelOutput = TRUE)
      req(nchar(input$lab_no) == 8, cancelOutput = TRUE)
      
      specimen <- dbase_specimen %>% 
        tbl("specimen_info") %>% 
        filter(lab_no == !!input$lab_no) %>% 
        collect()
      
      if(nrow(specimen) == 0){
        
        #validate(glue::glue("Lab no '{input$lab_no}' was not identified"))
        show_toast("warning", "", "No Such Lab No")
        shinyjs::reset("lab_no")
        return()
        
      }
      
      sample_info <- dbase_specimen %>% 
        tbl("sample_info") %>% 
        filter(unique_id == !!specimen$unique_id) %>% 
        collect()
      
      shinyjs::reset("lab_no")
      
      show_toast("success", "", "Lab No found")
      
      rv$specimen_selected <- specimen
      
      rv$sample_selected <- sample_info
      
      
    })
    
    # Specimen Information UI ----
    
    output$lab_noUI <- renderUI({
      
      
      req(rv$specimen_selected , cancelOutput = TRUE)
      
      session$userData$db_trigger()
      
      # get it from the database, so that this UI will reflect
      # any changes in the DB
      specimen <- dbase_specimen %>% 
        tbl("specimen_info") %>% 
        filter(lab_no == !!rv$specimen_selected$lab_no) %>% 
        collect()
      
      
      if(nrow(specimen) == 0) validate("Oups! Something went wrong. Contact support!")
      
      
      box(width = NULL,
          tagList(
            h3("Specimen Information"),
            hr(),
            p("Lab no: ", strong(specimen$lab_no)),
            p("Quality of Sample: ", 
              strong(specimen$quality), mod_edit_specimen_button_ui(ns("quality")) ),
            p("Specimen Type: ", 
              strong(specimen$specimen_type), mod_edit_specimen_button_ui(ns("specimen_type")) 
            ),
            p(span("Freezer: ", 
                   strong(specimen$freezer) , " - Storage Place: ", strong(specimen$place)),
              mod_edit_specimen_button_ui(ns("place"))
            ),
            p("Comments: ", 
              strong(specimen$comment_place), mod_edit_specimen_button_ui(ns("comment_place")) 
            ),
            p("Date processing: ", strong(to_date_time(specimen$date_processing) %>% format("%d/%m/%Y %H:%M")),
              mod_edit_specimen_button_ui(ns("date_processing"))
            ),
            p(col_labels[["duration"]], ": ",  strong(specimen$duration)),
            p(col_labels[["n_tubes"]], ": ",  
              strong(specimen$n_tubes), mod_edit_specimen_button_ui(ns("n_tubes") )
            )
          )  
      )
      
    })
    
    mod_edit_specimen_button_server("quality", reactive(rv$specimen_selected))
    mod_edit_specimen_button_server("comment_place", reactive(rv$specimen_selected))
    mod_edit_specimen_button_server("n_tubes", reactive(rv$specimen_selected))
    mod_edit_specimen_button_server("date_processing", reactive(rv$specimen_selected))
    mod_edit_specimen_button_server("place", reactive(rv$specimen_selected))
    
    res <- mod_edit_specimen_button_server("specimen_type", reactive(rv$specimen_selected))
    
    observeEvent(res$trigger, {
      
      updateTextInput(session, "lab_no", value = res$new_lab_no )
    },ignoreInit = TRUE)
    
    # Sample Information UI ----
    output$sample_infoUI <- renderUI({
      
      
      req(rv$specimen_selected , cancelOutput = TRUE)
      
      session$userData$db_trigger()
      
      # get it from the database, so that this UI will reflect
      # any changes in the DB
      sample_info <- dbase_specimen %>% 
        tbl("sample_info") %>% 
        filter(unique_id == !!rv$sample_selected$unique_id) %>% 
        collect()
      
      if(nrow(sample_info) == 0) validate("Oups! Something went wrong. Contact support!")
      
      
      box(width = NULL,
          tagList(
            h3("Sample Information"),
            hr(),
            p("Patient name: ", 
              strong(sample_info$firstname), mod_edit_sample_button_ui(ns("firstname")), " ", 
              strong(sample_info$surname), mod_edit_sample_button_ui(ns("surname"))
            ),
            
            p(col_labels[["bococ"]], ": ", strong(sample_info$bococ), 
              mod_edit_sample_button_ui(ns("bococ")),
              " | ", col_labels[["civil_id"]], ": ", strong(sample_info$civil_id),
              mod_edit_sample_button_ui(ns("civil_id"))
            ),
            
            p(col_labels[["gender"]], ": ", strong(sample_info$gender),
              mod_edit_sample_button_ui(ns("gender"))
            ),
            
            p(col_labels[["dob"]],  ": ",strong(to_date(sample_info$dob)),
              mod_edit_sample_button_ui(ns("dob"))
            ),
            
            p(col_labels[["nationality"]], ": ", strong(sample_info$nationality),
              mod_edit_sample_button_ui(ns("nationality"))
            ),
            p(col_labels[["diagnosis"]], ": ", strong(sample_info$diagnosis),
              mod_edit_sample_button_ui(ns("diagnosis"))),
            
            p(col_labels[["status"]],  ": ",strong(sample_info$status),
              mod_edit_sample_button_ui(ns("status"))
            ),
            
            p(col_labels[["doctor"]], ": ",strong(sample_info$doctor),
              mod_edit_sample_button_ui(ns("doctor"))
            ),
            
            p(col_labels[["consent"]], ": ",strong(sample_info$consent),
              mod_edit_sample_button_ui(ns("consent"))
            ),
            
            p(col_labels[["type1"]], ": ",strong(sample_info$type1),
              mod_edit_sample_button_ui(ns("type1"))
            ),
            
            p(col_labels[["type1_ml"]], ": ",strong(sample_info$type1_ml),
              mod_edit_sample_button_ui(ns("type1_ml"))
            ),
            
            p(col_labels[["phase"]], ": ", strong(sample_info$phase),
              mod_edit_sample_button_ui(ns("phase"))
            ),
            
            p(col_labels[["date_collection"]], ": ", strong(to_date_time(sample_info$date_collection) %>% format("%d/%m/%Y %H:%M")),
              mod_edit_sample_button_ui(ns("date_collection"))
            ),
            
            p(col_labels[["tube"]], ": ", strong(sample_info$tube),
              mod_edit_sample_button_ui(ns("tube"))
            ),
            p(col_labels[["at_bococ"]], ": ", strong(sample_info$at_bococ),
              mod_edit_sample_button_ui(ns("at_bococ"))
            ),
            
            p(col_labels[["date_shipment"]], ": ", strong(to_date(sample_info$date_shipment) %>% format("%d/%m/%Y")),
              mod_edit_sample_button_ui(ns("date_shipment"))
            ),
            
            p(col_labels[["date_receipt"]], ": ", strong(to_date_time(sample_info$date_receipt) %>% format("%d/%m/%Y %H:%M")),
              mod_edit_sample_button_ui(ns("date_receipt"))
            ),
            
            p(col_labels[["study_id"]], ": ", strong(sample_info$study_id),
              mod_edit_sample_button_ui(ns("study_id"))
            ),
            p(col_labels[["study"]], ": ", strong(sample_info$study),
              mod_edit_sample_button_ui(ns("study"))
            ),
            p(col_labels[["path_icf"]], ": ", strong(sample_info$path_icf)
            ),
            p(col_labels[["comments"]], ": ", strong(sample_info$comments),
              mod_edit_sample_button_ui(ns("comments"))
            ),
            # does not change. It is defined by how many specimens the user has input
            # when first fill ine the sample Info form and then the specimens
            p(col_labels[["specimens"]], ": ", strong(sample_info$specimens)
              #,mod_edit_sample_button_ui(ns("specimens"))
            )
          )
      )  
      
      
    })
    
    
    mod_edit_sample_button_server("firstname", reactive(rv$sample_selected))
    mod_edit_sample_button_server("surname", reactive(rv$sample_selected))
    mod_edit_sample_button_server("status", reactive(rv$sample_selected))
    mod_edit_sample_button_server("doctor", reactive(rv$sample_selected))
    mod_edit_sample_button_server("diagnosis", reactive(rv$sample_selected))
    mod_edit_sample_button_server("consent", reactive(rv$sample_selected))
    
    mod_edit_sample_button_server("type1", reactive(rv$sample_selected))
    mod_edit_sample_button_server("type1_ml", reactive(rv$sample_selected))
    
    mod_edit_sample_button_server("phase", reactive(rv$sample_selected))
    mod_edit_sample_button_server("study_id", reactive(rv$sample_selected))
    mod_edit_sample_button_server("study", reactive(rv$sample_selected))
    mod_edit_sample_button_server("bococ", reactive(rv$sample_selected))
    mod_edit_sample_button_server("civil_id", reactive(rv$sample_selected))
    mod_edit_sample_button_server("gender", reactive(rv$sample_selected))
    mod_edit_sample_button_server("tube", reactive(rv$sample_selected))
    mod_edit_sample_button_server("at_bococ", reactive(rv$sample_selected))
    mod_edit_sample_button_server("nationality", reactive(rv$sample_selected))
    mod_edit_sample_button_server("dob", reactive(rv$sample_selected))
    mod_edit_sample_button_server("comments", reactive(rv$sample_selected))
    
    mod_edit_sample_button_server("icf", reactive(rv$sample_selected))
    
    mod_edit_sample_button_server("date_collection", reactive(rv$sample_selected))
    mod_edit_sample_button_server("date_shipment", reactive(rv$sample_selected))
    mod_edit_sample_button_server("date_receipt", reactive(rv$sample_selected))
    
  })
}

## To be copied in the UI
# mod_view_edit_specimen_ui("view_edit_specimen_1")

## To be copied in the server
# mod_view_edit_specimen_server("view_edit_specimen_1")
