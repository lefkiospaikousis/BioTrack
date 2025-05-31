#' sample_type UI Function
#'
#' @description Collects the information of a sample 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_add_sample_type_ui <- function(id){
  ns <- NS(id)
  
  input_width <- "80%"
  
  
  tagList(
    div(style = "font-size:13px",
        h4("3. Sample Collection information:"),
        hr(style = "width: 80%"),
        tags$table(
          tags$tr(width = "100%",
                  tags$td(width = "40%", div(class = "input-label",style = "", "Sample Type Received:")),
                  tags$td(width = "60%", selectInput(ns("type1"), NULL,  c("", col_values[["sample_types"]]), width = input_width))
          ),
          
          tags$tr(width = "100%", id = ns('sample_ml'),
                  tags$td(width = "30%", div(class = "input-label",style = "", "ml:")),
                  tags$td(width = "70%", numericInput(ns("type1_ml"), NULL, NA, width = input_width))
          ),
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label",style = "", HTML("Type of <br>Collection tube"))),
                  tags$td(width = "70%", selectInput(ns("tube"), NULL, c("", col_values[["tube"]]), width = input_width))
          ),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label",style = "", col_labels[["phase"]])),
                  tags$td(width = "70%", selectInput(ns("phase"), NULL, NULL, width = input_width))
          ),
          tags$tr(width = "100%",
                  tags$td(width = "30%",  shinyjs::hidden(div(class = "input-label", style = "", "Other:"))),
                  tags$td(width = "70%",  shinyjs::hidden(textInput(ns("phase_other"), NULL, width = input_width,
                                                                    placeholder = "Please describe")))),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label",style = "", col_labels[["lab"]])),
                  tags$td(width = "70%", selectInput(ns("lab"), NULL, c("", col_values[["lab"]]), width = input_width))),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%",  shinyjs::hidden(div(class = "input-label", style = "", ""))),
                  tags$td(width = "70%",  shinyjs::hidden(textInput(ns("lab_other"), NULL, width = input_width,
                                                                    placeholder = "Please type")))),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label",style = "", "Collected at BOCOC?:")),
                  tags$td(width = "70%", selectInput(ns("at_bococ"), NULL, c("", col_values[["at_bococ"]]), width = input_width))),
          
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label", "Date of collection:")),
                  tags$td(width = "70%", 
                          splitLayout(cellWidths = c("50%", "60%"),
                                      div(dateInput(ns("date_collection"), NULL, lubridate::NA_Date_ , format = "dd/mm/yyyy", width = input_width)),
                                      div(style = "margin-top: 0px; margin-left:-20px" , 
                                          shinyTime::timeInput(ns("time_collection"), NULL, seconds = FALSE))
                          )
                  )
          ),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label", "Date of shipment:")),
                  tags$td(width = "70%", dateInput(ns("date_shipment"), NULL, lubridate::NA_Date_ , format = "dd/mm/yyyy", width = input_width))
          ),
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label",style = "", HTML("Date & Time<br>of receipt"))),
                  tags$td(width = "70%", 
                          
                          splitLayout(cellWidths = c("50%", "60%"),
                                      div(dateInput(ns("date_receipt"), NULL, lubridate::NA_Date_ , format = "dd/mm/yyyy", width = input_width)),
                                      div(style = "margin-top: 0px; margin-left:-20px" , shinyTime::timeInput(ns("time_receipt"), NULL, seconds = FALSE)),
                          )
                  )
          ),
          
          # FFPE specific fields (initially hidden)
          shinyjs::hidden(
            tags$tr(width = "100%", id = ns("ffpe_field_origin"),
                    tags$td(width = "30%", div(class = "input-label",style = "", col_labels[["sample_origin"]])),
                    tags$td(width = "70%", selectInput(ns("sample_origin"), NULL, c("", col_values[["sample_origin"]]), width = input_width))
            ),
            tags$tr(width = "100%", id = ns("ffpe_field_location"),
                    tags$td(width = "30%", div(class = "input-label", style = "")),
                    tags$td(width = "70%", textInput(ns("location_lesion"), NULL, width = input_width,
                                                     value = "", placeholder = col_labels[["location_lesion"]]))
            ),
            tags$tr(width = "100%", id = ns("ffpe_field_anatomical"),
                    tags$td(width = "30%", div(class = "input-label",style = "", col_labels[["anatomical_site"]], br(), '(if bilateral tumour please specify laterality)')),
                    tags$td(width = "70%", textInput(ns("anatomical_site"), NULL, width = input_width, placeholder = ""))
            ),
            tags$tr(width = "100%", id = ns("ffpe_field_technique"),
                    tags$td(width = "30%", div(class = "input-label",style = "", col_labels[["sampling_technique"]])),
                    tags$td(width = "70%", selectInput(ns("sampling_technique"), NULL, c("", col_values[["sampling_technique"]]), width = input_width))
            ),
            tags$tr(width = "100%", id = ns("ffpe_field_technique_other"),
                    tags$td(width = "30%", div(class = "input-label", style = "")),
                    tags$td(width = "70%", textInput(ns("sampling_technique_other"), NULL, width = input_width,
                                                     placeholder = 'Please describe'))
            )
            
          )
        )
    ),
    hr(),
    actionButton(ns("submit"), "Done", class = "btn-submit",
                 icon("glyphicon glyphicon-ok", lib = "glyphicon")),
    actionButton(ns('cancel'), "Cancel", class = "btn-cancel right", 
                 icon("glyphicon glyphicon-remove", lib = "glyphicon")),
    hr()
  )
}

#' sample_type Server Functions
#'
#' @noRd 
mod_add_sample_type_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    submitted <- reactiveVal(0)
    close_form <- reactiveVal(0)
    
    all_fields <- c(
      'type1',
      'type1_ml',
      "tube",
      "phase",
      "lab",
      "at_bococ",
      "date_collection",
      "time_collection",
      "date_shipment",
      
      # FFPE specific fields
      "sample_origin",
      "location_lesion",
      "anatomical_site",
      "sampling_technique",
      "sampling_technique_other",
      
      "date_receipt",
      "time_receipt"
    )
    
    # Show/hide FFPE fields based on type1 selection
    observe({
      
      req(input$type1)
      is_ffpe <- input$type1 %in% sample_types_FFPE
      
      if(is_ffpe) {
        shinyjs::show("ffpe_field_origin")
        #shinyjs::show("ffpe_field_location")
        shinyjs::show("ffpe_field_anatomical")
        shinyjs::show("ffpe_field_technique")
        
        updateNumericInput(session, "type1_ml", value = NA)
        shinyjs::disable("sample_ml")
        
        updateSelectInput(session, "phase", choices = c("", phase_FFPE))
        
        # # Only show the technique_other field if "Other" is selected
        # if(input$sampling_technique == "Other") {
        #   shinyjs::show("ffpe_field_technique_other")
        # } else {
        #   shinyjs::hide("ffpe_field_technique_other")
        # }
        
      } else {
        shinyjs::hide("ffpe_field_origin")
        shinyjs::hide("ffpe_field_location")
        shinyjs::hide("ffpe_field_anatomical")
        shinyjs::hide("ffpe_field_technique")
        shinyjs::hide("ffpe_field_technique_other")
        
        shinyjs::enable("sample_ml")
        
        non_ffpe <- col_values[["phase"]] |> purrr::discard(~ . %in% phase_FFPE) |> c('Other')
        
        updateSelectInput(session, "phase", choices = c("", non_ffpe))
      }
    }) |> 
      bindEvent(input$type1)
    
    # Validation ----
    
    iv <- shinyvalidate::InputValidator$new()
    
    iv$add_rule("type1", sv_required())
    
    #4. Collection information
    iv$add_rule("tube", sv_required())
    iv$add_rule("phase", sv_required())
    iv$add_rule("at_bococ", sv_required())
    iv$add_rule("date_collection", sv_required())
    iv$add_rule("date_collection",  ~valid_date(., "Collection date"))
    
    iv$add_rule("date_receipt", sv_required())
    iv$add_rule("date_receipt",  ~valid_date(., "Receipt date"))
    
    iv$add_rule("time_receipt", function(time){
      if(identical(strftime(time, "%R"), "00:00")){
        "Required"
      }
    })
    
    iv_date_shipment <- shinyvalidate::InputValidator$new()
    iv_date_shipment$condition(~ input$at_bococ == 'No')
    
    iv_date_shipment$add_rule("date_shipment", sv_required())
    iv_date_shipment$add_rule("date_shipment", ~valid_date(., "Shipment date"))
    
    iv$add_validator(iv_date_shipment)
    
    # Add validation for FFPE specific fields
    iv_ffpe_fields <- shinyvalidate::InputValidator$new()
    iv_ffpe_fields$condition(~ input$type1 %in% sample_types_FFPE)
    
    iv_ffpe_fields$add_rule("sample_origin", sv_required())
    iv_ffpe_fields$add_rule("anatomical_site", sv_required())
    iv_ffpe_fields$add_rule("sampling_technique", sv_required())
    
    # Validation for sample origin in case of lesion
    iv_sample_origin <- shinyvalidate::InputValidator$new()
    iv_sample_origin$condition(~ input$type1 %in% sample_types_FFPE && input$sample_origin == "Metastatic lesion")
    iv_sample_origin$add_rule("location_lesion", sv_required())
    
    # Validation for sampling_technique_other
    iv_sampling_other <- shinyvalidate::InputValidator$new()
    iv_sampling_other$condition(~ input$type1 %in% sample_types_FFPE && input$sampling_technique == "Other")
    iv_sampling_other$add_rule("sampling_technique_other", sv_required())
    
    iv_ffpe_fields$add_validator(iv_sampling_other)
    iv_ffpe_fields$add_validator(iv_sample_origin)
    
    iv$add_validator(iv_ffpe_fields)
    
    observe({
      shinyjs::toggle("phase_other", anim = TRUE, condition = input$phase == "Other")
    })
    
    observe({
      shinyjs::toggle("lab_other", anim = TRUE, condition = input$lab == "Other")
    })
    
    observe({
      shinyjs::toggle("ffpe_field_technique_other", anim = TRUE, condition = input$sampling_technique == "Other")
    })
    
    observe({
      shinyjs::toggle("ffpe_field_location", anim = TRUE, condition = input$sample_origin == "Metastatic lesion")
    })
    
    
    observe({
      shinyjs::toggle("original_fields", anim = TRUE, condition = !input$type1 %in% sample_types_FFPE)
    })
    
    
    # Data collection ----
    
    form_data <- reactive({
      
      all_fields <- unname(all_fields) %>% setNames(all_fields)
      
      list_dta <- map(all_fields, function(x) {
        if(length(input[[x]]) > 1) {
          paste(input[[x]], collapse = ", ")
        } else {
          input[[x]]
        }
        
      })
      
      # If Date(0) object or zero length dateTime object, then needs to be NA_Date_
      # otherwise cannot be save in the DB. lubridate::is.timepoint works for both Date and Posixct objects
      # So if time_processing is not there, this will make sure all is good for saving in the DB
      list_dta <- list_dta %>% 
        purrr::map_if(lubridate::is.timepoint, function(x){
          
          if(length(x) == 0) {
            lubridate::NA_Date_
          } else {
            x
          }
        }) 
      
      if(input$phase == "Other") list_dta$phase <- paste0("Other:", input$phase_other)
      if(input$lab == "Other") list_dta$lab <- input$lab_other
      if(input$sampling_technique == "Other") list_dta$sampling_technique <- paste0("Other:", input$sampling_technique_other)
      
      # Only include FFPE fields if relevant
      if(!input$type1 %in% sample_types_FFPE) {
        list_dta$sample_origin <- NA
        list_dta$location_lesion <- NA
        list_dta$anatomical_site <- NA
        list_dta$sampling_technique <- NA
        list_dta$sampling_technique_other <- NA
      } 
      
      
      list_dta
      
    }) %>% 
      bindEvent(input$submit)
    
    
    observeEvent(input$cancel, {
      
      iv$disable() # so when the user enters again, it finds all the same 
      close_form(close_form()+1)
      
    })
    
    
    observeEvent(input$submit, {
      
      if (iv$is_valid()) {
        
        iv$disable()
        removeNotification("submit_message")
        
        submitted(submitted()+1)
        
      } else {
        iv$enable() # Start showing validation feedback
        
        #if(golem::app_dev()) submitted(submitted()+1)
        showNotification(
          "Please correct the errors in the form and try again",
          id = "submit_message", type = "error")
      }
      
      
    })
    
    
    # Return ####
    return(
      
      list(
        dta     = form_data,
        submit  = submitted,
        cancel = close_form
      )
    )
    
  })
}

## To be copied in the UI
# mod_add_sample_type_ui("add_sample_type_1")

## To be copied in the server
# mod_add_sample_type_server("add_sample_type_1")
