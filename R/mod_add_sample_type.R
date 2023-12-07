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
    splitLayout(
      div(style = "font-size:13px",
          h4("3. Sample Collection information:"),
          hr(style = "width: 80%"),
          tags$table(
            tags$tr(width = "100%",
                    tags$td(width = "5%", div(class = "input-label",style = "", "Sample Type Received:")),
                    tags$td(width = "50%", 
                            
                            splitLayout(cellWidths = c("55%", "60%"),
                                        div(selectInput(ns("type1"), NULL, c("", col_values[["sample_types"]]), width = input_width)),
                                        div( style = "margin-top: 0px; margin-left:-20px" , numericInput(ns("type1_ml"), NULL, NA, width = "50%"))
                            ) #div(class = "input-label2", style = "", "ml:"),
                    )
            ),
            tags$tr(width = "100%",
                    tags$td(width = "40%", div(class = "input-label",style = "", HTML("Type of <br>Collection tube"))),
                    tags$td(width = "60%", selectInput(ns("tube"), NULL, c("", col_values[["tube"]]), width = input_width))),
            
            tags$tr(width = "100%",
                    tags$td(width = "30%", div(class = "input-label",style = "", col_labels[["phase"]])),
                    tags$td(width = "70%", selectInput(ns("phase"), NULL, c("", col_values[["phase"]]), width = input_width))),
            
            tags$tr(width = "100%",
                    tags$td(width = "30%",  shinyjs::hidden(div(class = "input-label", style = "", "Other:"))),
                    tags$td(width = "70%",  shinyjs::hidden(textInput(ns("phase_other"), NULL, width = input_width,
                                                                      placeholder = "Please describe")))),
            
            
            tags$tr(width = "100%",
                    tags$td(width = "30%", div(class = "input-label",style = "", "Collected at BOCOC?:")),
                    tags$td(width = "70%", selectInput(ns("at_bococ"), NULL, c("", col_values[["at_bococ"]]), width = input_width))),
            
            
            tags$tr(width = "100%",
                    tags$td(width = "30%", div(class = "input-label", "Date of collection:")),
                    tags$td(width = "70%", 
                            splitLayout(cellWidths = c("50%", "60%"),
                                        div(dateInput(ns("date_collection"), NULL, lubridate::NA_Date_ , format = "dd/mm/yyyy", width = "80%")),
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
                    tags$td(width = "40%", div(class = "input-label",style = "", HTML("Date & Time<br>of receipt"))),
                    tags$td(width = "60%", 
                            
                            splitLayout(cellWidths = c("50%", "60%"),
                                        div(dateInput(ns("date_receipt"), NULL, lubridate::NA_Date_ , format = "dd/mm/yyyy", width = "80%")),
                                        div(style = "margin-top: 0px; margin-left:-20px" , shinyTime::timeInput(ns("time_receipt"), NULL, seconds = FALSE)),
                            )
                    )
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
      "at_bococ",
      "date_collection",
      "time_collection",
      "date_shipment",
      
      "date_receipt",
      "time_receipt"
    )
    
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
    
    
    
    observe({
      
      shinyjs::toggle("phase_other", anim = TRUE, condition = input$phase == "Other")
      
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
        
        if(golem::app_dev()) submitted(submitted()+1)
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
