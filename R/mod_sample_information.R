#' sample_information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @import dplyr
#' @importFrom purrr map map2 pmap keep walk
#' @importFrom shinyWidgets awesomeCheckboxGroup prettyRadioButtons airDatepickerInput
#' @importFrom shinyvalidate sv_required sv_optional sv_gt sv_gte
#' @importFrom shiny NS tagList 
#' @importFrom lubridate ymd hm hms
mod_sample_information_ui <- function(id){
  ns <- NS(id)
  
  input_width <- "80%"
  
  tagList(div(id = ns("form"),
              
              splitLayout(
                
                div(style = "font-size:13px",
                    h4("1.Contact Information:"),
                    hr(style = "width: 80%"),
                    tags$table(
                      tags$tr(width = "100%",
                              tags$td(width = "40%", div(class = "input-label",style = "", "Patient First Name:")),
                              tags$td(width = "60%", textInput(ns("firstname"), NULL, width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label",style = "", "Patient Surname:")),
                              tags$td(width = "70%", textInput(ns("surname"), NULL, NA, width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label",style = "", "Gender:")),
                              tags$td(width = "70%", selectInput(ns("gender"), NULL, c("", col_values[["gender"]]), width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label",style = "", "BOCOC ID:")),
                              tags$td(width = "70%", numericInput(ns("bococ"), NULL, NA, min = 1, width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label",style = "", "Civil ID:")),
                              tags$td(width = "70%", textInput(ns("civil_id"), NULL, NA, width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label", "Date of birth:")),
                              tags$td(width = "70%", dateInput(ns("dob"), NULL, as.Date(NA), format = "dd/mm/yyyy", width = input_width))),
                      
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label",style = "", "Nationality:")),
                              tags$td(width = "70%", textInput(ns("nationality"), NULL, NA, width = input_width))),
                      
                    )
                ),
                div(style = "font-size:13px",
                    h4("2.Clinical Information:"),
                    hr(style = "width: 80%"),
                    tags$table(
                      tags$tr(width = "100%",
                              tags$td(width = "40%", div(class = "input-label",style = "", "Diagnosis:")),
                              tags$td(width = "60%", textInput(ns("diagnosis"), NULL, NA, width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label",style = "", "Status:")),
                              tags$td(width = "70%", selectInput(ns("status"), NULL, c("", col_values[["status"]]), width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label",style = "", "Referring doctor")),
                              tags$td(width = "70%", textInput(ns("doctor"), NULL, NA, width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label",style = "", "Consent Signed:")),
                              tags$td(width = "70%", selectInput(ns("consent"), NULL, c("", col_values[["consent"]]), width = input_width))),
                      
                    )
                )
              ),
              
              br(),
              br(),
              
              splitLayout(
                div(style = "font-size:13px",
                    h4("3.Sample Type received:"),
                    hr(style = "width: 80%"),
                    tags$table(
                      tags$tr(width = "100%",
                              tags$td(width = "5%", div(class = "input-label",style = "", "1.:")),
                              tags$td(width = "50%", selectInput(ns("type1"), NULL, c("", col_values[["sample_types"]]), width = input_width)),
                              tags$td(div(class = "input-label2", style = "", "ml:")),
                              tags$td(numericInput(ns("type1_ml"), NULL, NA, width = "50%"))),
                      
                      # tags$tr(width = "100%",
                      #         tags$td(width = "5%", div(class = "input-label",style = "", "2.:")),
                      #         tags$td(width = "50%", selectInput(ns("type2"), NULL, c("", col_values[["sample_types"]]), width = input_width)),
                      #         tags$td(div(class = "input-label2", style = "", "ml:")),
                      #         tags$td( numericInput(ns("type2_ml"), NULL, NA, width = "50%"))),
                      # 
                      # tags$tr(width = "100%",
                      #         tags$td(width = "5%", div(class = "input-label",style = "", "3.:")),
                      #         tags$td(width = "50%", selectInput(ns("type3"), NULL, c("", col_values[["sample_types"]]), width = input_width)),
                      #         tags$td(div(class = "input-label2", style = "", "ml:")),
                      #         tags$td(numericInput(ns("type3_ml"), NULL, NA, width = "50%"))),
                      # 
                      # tags$tr(width = "100%",
                      #         tags$td(width = "5%", div(class = "input-label",style = "", "4.:")),
                      #         tags$td(width = "50%", selectInput(ns("type4"), NULL, c("", col_values[["sample_types"]]), width = input_width)),
                      #         tags$td(div(class = "input-label2", style = "", "ml:")),
                      #         tags$td(numericInput(ns("type4_ml"), NULL, NA, width = "50%"))),
                      # 
                      # tags$tr(width = "100%",
                      #         tags$td(width = "5%", div(class = "input-label",style = "", "5.:")),
                      #         tags$td(width = "50%", selectInput(ns("type5"), NULL, c("", col_values[["sample_types"]]), width = input_width)),
                      #         tags$td(div(class = "input-label2", style = "", "ml:")),
                      #         tags$td(numericInput(ns("type5_ml"), NULL, NA, width = "50%")))
                      
                      
                    )
                    
                ),
                
                div(style = "font-size:13px",
                    h4("4.Collection information:"),
                    hr(style = "width: 80%"),
                    tags$table(
                      
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
                                      splitLayout(cellWidths = c("55%", "60%"),
                                                  div(dateInput(ns("date_collection"), NULL, lubridate::NA_Date_ , format = "dd/mm/yyyy", width = "80%")),
                                                  div(style = "margin-top: 0px; margin-left:-20px" , 
                                                      shinyTime::timeInput(ns("time_collection"), NULL, seconds = FALSE))
                                      )
                                      
                              )),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label", "Date of shipment:")),
                              tags$td(width = "70%", dateInput(ns("date_shipment"), NULL, lubridate::NA_Date_ , format = "dd/mm/yyyy", width = input_width))),
                      
                    )
                )
              ),
              hr(),
              
              
              splitLayout(
                div(style = "font-size:13px",
                    h4("BOCOC Lab use only"),
                    #hr(style = "width: 80%"),
                    tags$table(
                      
                      
                      tags$tr(width = "100%",
                              tags$td(width = "40%", div(class = "input-label",style = "", "Patient Study ID")),
                              tags$td(width = "60%", textInput(ns("study_id"), NULL, NA, width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "30%", div(class = "input-label",style = "", "Study involved")),
                              tags$td(width = "70%", textInput(ns("study"), NULL, NA, width = input_width))),
                      
                      tags$tr(width = "100%",
                              tags$td(width = "40%", div(class = "input-label",style = "", HTML("Date & Time<br>of receipt"))),
                              tags$td(width = "60%", 
                                      
                                      splitLayout(cellWidths = c("40%", "50%"),
                                                  div(dateInput(ns("date_receipt"), NULL, lubridate::NA_Date_ , format = "dd/mm/yyyy", width = "90%")),
                                                  div(style = "margin-top: 0px; margin-left:-5px" , shinyTime::timeInput(ns("time_receipt"), NULL, seconds = FALSE)),
                                      )
                              ))
                    )
                ),
                div(style = "font-size:13px",
                    textAreaInput(ns("comments"), h4("Comments"), rows = 5, width = "80%", resize = "both")
                )
                
              )
  ),
  fileInput(ns("icf"), "Upload the scanned PDF file of the ICF", multiple = TRUE, accept = ".pdf"),
  
  hr(),
  actionButton(ns("submit"), "Submit", class = "btn-submit", width = "100%",
               icon("glyphicon glyphicon-ok", lib = "glyphicon")),
  # actionButton(ns('cancel'), "Cancel", class = "btn-cancel right", 
  #              icon("glyphicon glyphicon-remove", lib = "glyphicon")),
  hr()
  
  
  )
}

#' sample_information Server Functions
#'
#' @noRd 
mod_sample_information_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    submitted <- reactiveVal(0)
    cancel <- reactiveVal(0)
    
    uploaded_icf <- reactiveVal(FALSE)
    rv <- reactiveValues(icf_path = NULL)
    
    observe({
      
      shinyjs::toggle("phase_other", anim = TRUE, condition = input$phase == "Other")
      
    })
    
    # Fields. Collect the input ids
    all_fields <- 
      c(
        "firstname",
        "surname",
        "gender",
        "bococ",
        "dob",
        "nationality",
        "diagnosis",
        "status",
        "doctor",
        "consent",
        # paste0("type", 1:5),
        # paste0("type", 1:5, "_ml"),
        'type1',
        'type1_ml',
        "tube",
        "phase",
        "at_bococ",
        "date_collection",
        "time_collection",
        "date_shipment",
        
        # "date_processing",
        # "time_processing",
        "date_receipt",
        "time_receipt",
        "civil_id",
        "study_id",
        "study",
        "comments"
        
      )
    
    # Validation ----
    
    iv <- shinyvalidate::InputValidator$new()
    
    #1. Contact information
    iv$add_rule("firstname", sv_required())
    iv$add_rule("surname", sv_required())
    
    iv$add_rule("gender", sv_required())
    
    iv$add_rule("bococ", sv_gte(1))
    iv$add_rule("bococ", valid_bococ)
    
    iv$add_rule("civil_id", sv_required())
    
    iv$add_rule("dob", sv_required())
    iv$add_rule("dob", ~valid_date(., "DOB"))
    
    iv$add_rule("nationality", sv_required())
    
    #2. Clinical Information
    iv$add_rule("diagnosis", sv_required())
    #iv$add_rule("status", sv_required())
    iv$add_rule("doctor", sv_required())
    iv$add_rule("consent", sv_required())
    
    #3. Sample type
    # only 1 required
    iv$add_rule("type1", sv_required())
    
    #4. Collection information
    iv$add_rule("tube", sv_required())
    iv$add_rule("phase", sv_required())
    iv$add_rule("at_bococ", sv_required())
    iv$add_rule("date_collection", sv_required())
    iv$add_rule("date_collection",  ~valid_date(., "Collection date"))
    
    iv_date_shipment <- shinyvalidate::InputValidator$new()
    iv_date_shipment$condition(~ input$at_bococ == 'No')
    
    iv_date_shipment$add_rule("date_shipment", sv_required())
    iv_date_shipment$add_rule("date_shipment", ~valid_date(., "Shipment date"))
    
    iv$add_validator(iv_date_shipment)
    
    
    iv$add_rule("date_receipt", sv_required())
    iv$add_rule("date_receipt",  ~valid_date(., "Receipt date"))
    
    iv$add_rule("time_receipt", function(time){
      if(identical(strftime(time, "%R"), "00:00")){
        "Required"
      }
    })
    
    
    #iv$add_rule("date_processing", sv_required())
    iv$add_rule("study_id", sv_required())
    iv$add_rule("study", sv_required())
    
    form_data <- reactive({
      
      # Temporary collection of the data - see later final_forma_data
      
      # To keep the output ids as column names - short&sweet and then
      # then possible rename using the same vector, when presenting
      all_fields <- unname(all_fields) %>% setNames(all_fields)
      
      list_dta <- map(all_fields, function(x) {
        
        # Because the Checks1 input maybe have more than one value and then it goes to 
        # more than one row in the table. I need one row per submission
        if(length(input[[x]]) > 1) {
          paste(input[[x]], collapse = ", ")
        } else {
          input[[x]]
        }
        
      }) 
      
      if(input$phase == "Other") list_dta$phase <- paste0("Other:", input$phase_other)
      
      list_dta
      
    }) %>% 
      bindEvent(input$submit)
    
    
    observeEvent(input$submit, {
      
      if (iv$is_valid()) {
        
        iv$disable()
        removeNotification("submit_message")
        
        
        
        if(isFALSE(uploaded_icf())) {
          show_toast("error", "One more thing", "You need to upload an ICF form")
        } else {
          
          shinyjs::reset("form")
          submitted(submitted() + 1)
          
        }
        
        
      } else {
        
        iv$enable() # Start showing validation feedback
        
        if(golem::app_dev()) submitted(submitted() + 1) # only in dev, I allow to proceed withou the validaiton
        
        showNotification(
          "Please correct the errors in the form and try again",
          id = "submit_message", type = "error")
      }
      
      
    })
    
    observeEvent(input$cancel, {
      
      cancel(cancel() + 1)
    })
    
    
    observeEvent(input$icf, {
      
      
      if ( !all(tools::file_ext(input$icf$name) == "pdf")) {
        shinyFeedback::hideFeedback("icf")
        shinyFeedback::showFeedbackDanger("icf", "One or more of the files are not .pdf documents")
        
      } else {
        
        shinyFeedback::hideFeedback("icf")
        shinyFeedback::showFeedbackSuccess("icf", "Succesfull upload!")
        
        # Store the temporary path where the file was saved
        rv$icf_path <- input$icf
        uploaded_icf(TRUE)
      }
      
      
    }, ignoreInit = TRUE)
    
    
    return(
      
      list(
        dta      = form_data,
        icf_path = reactive(rv$icf_path),
        submit   = submitted
        #cancel  = cancel
      )
    )
    
    
  })
}

## To be copied in the UI
# mod_sample_information_ui("sample_information_1")

## To be copied in the server
# mod_sample_information_server("sample_information_1")
