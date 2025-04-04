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
              actionButton(ns("add_sample"), "Add sample", icon = icon("plus")),
              tableOutput(ns("tbl_samples")),
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
    
    samples <- reactiveValues()
    
    active_sample <- reactiveVal()
    
    output$tbl_samples <- renderTable({
      
      samples_list <- reactiveValuesToList(samples)
      
      req( length(samples_list) > 0 )
      
      samples_list |> 
        bind_rows() |>  
        mutate(
          across(c(date_collection, date_receipt), ~format(., "%d/%m/%Y - %H:%M"))
        ) |> 
        select(
          "Type"                      = type1,
          "ml"                        = type1_ml,
          "Tube"                      = tube,
          'Date & time of receipt'    = date_receipt, 
          'Date & time of collection' = date_collection,
          "Phase"                     = phase,
          "At BOCOC"                  = at_bococ
        )
      
    }, align = "c")
    
    observeEvent(input$add_sample, {
      
      if(isTRUE(is.na(input$bococ))){
        show_toast("error", "", "Please add a BOCOC number first")
        return()
      }
      
      showModal(modalDialog(
        mod_add_sample_type_ui(ns("add_sample_type_1"))
        , footer = NULL
      ))
      
    })
    
    # ADD SAMPLE information ---------------------------------------
    observeEvent(res_sample$cancel(), {
      
      removeModal()
      
    }, ignoreInit = TRUE)
    
    res_sample <- mod_add_sample_type_server("add_sample_type_1")
    
    observeEvent(res_sample$submit(), {
      
      sample <- process_sample( c(res_sample$dta(), list(bococ = input$bococ)) )
      
      samples[[sample$unique_id]] <- sample
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      #show_toast("success", "", glue::glue("Sample `{sample$type1}` successfully saved!"))
      
      active_sample(sample)
      
      
    }, ignoreInit = TRUE)
    
    
    # STORAGE ----------------------------------------------------------------
    
    observeEvent(active_sample(), {
      removeModal()
      
      show_waiter("Moving to Storage information.. Please wait", sleep = 1)
      waiter::waiter_hide()
      
      showModal(modalDialog(
        
        mod_storage_information_ui(ns("a_sample")),
        footer = NULL
      ))
      
    })
    
    res_storage <- mod_storage_information_server("a_sample", reactive( active_sample()) )
    
    observeEvent(res_storage$submit(), {
      
      hide_waiter()
      
      show_waiter("Saving the information.. Please wait", sleep = 1)
      id <- active_sample()$unique_id
      n_specimens <- nrow(res_storage$dta())
      
      processed_sample <- active_sample()
      processed_sample$specimens <- n_specimens
      
      rs <- DBI::dbAppendTable(dbase_specimen, "sample_info", as.data.frame(processed_sample))
      
      cat("Added sample information for ", rs, " sample \n")
      
      if(!golem::app_prod()) showNotification("Added sample information to DB")
      
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      
      hide_waiter()
      removeModal()
      
    }, ignoreInit = TRUE)
    

    # VALIDATION -----------------------------------------------------------
    
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
    iv$add_rule("doctor", sv_required())
    iv$add_rule("consent", sv_required())
    
    iv$add_rule("study_id", sv_required())
    iv$add_rule("study", sv_required())
    
    
    # COLLECT Form Data ------------------------------------------------------
    # Collect the input ids
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
        "civil_id",
        "study_id",
        "study",
        "comments"
        
      )
    
    form_data <- reactive({
      
      # Temporary collection of the data from the form
      # To keep the output ids as column names - short&sweet and then
      # then possible rename using the same vector, when presenting
      all_fields <- unname(all_fields) %>% setNames(all_fields)
      
      list_dta <- purrr::map(all_fields, function(x) {
        
        # Because the CheckBox input maybe have more than one value and then it goes to 
        # more than one row in the table. I need one row per submission
        if(length(input[[x]]) > 1) {
          paste(input[[x]], collapse = ", ")
        } else {
          input[[x]]
        }
        
      }) 
      
      list_dta
      
    }) %>% 
      bindEvent(input$submit)
    
    
    observeEvent(input$submit, {
      
      if (iv$is_valid()) {
        
        iv$disable()
        removeNotification("submit_message")
        
        if(isFALSE(uploaded_icf())) {
          show_toast("error", "One more thing", "You need to upload an ICF form")
          return()
        } 
        
        
        if( length(reactiveValuesToList(samples) ) == 0 ) {
          show_toast("error", "Hold on", "You haven't saved any samples yet")
          return()
        }
        
        submitted(submitted() + 1)
        
      } else {
        
        iv$enable() # Start showing validation feedback
        
        if(golem::app_dev()) submitted(submitted() + 1) # only in dev, I allow to proceed without the validaiton
        
        showNotification(
          "Please correct the errors in the form and try again",
          id = "submit_message", type = "error")
      }
      
      
    })
    
    observeEvent(input$cancel, {
      
      cancel(cancel() + 1)
      
    })
    
    # ICF UPLOAD -----------------------------------------------------------
    
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
    
    # RETURN Form DATA ----------------------------------------------------------
    
    return(
      
      list(
        dta      = form_data,
        samples  = samples,
        icf_path = reactive(rv$icf_path),
        submit   = submitted
      )
    )
    
    
  })
}

## To be copied in the UI
# mod_sample_information_ui("sample_information_1")

## To be copied in the server
# mod_sample_information_server("sample_information_1")
