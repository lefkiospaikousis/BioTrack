#' modify_specimen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_modal_modifyValue_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("info_ui")),
    actionButton(ns("submit"), "Done", class = "btn-submit",
                 icon("glyphicon glyphicon-ok", lib = "glyphicon")),
    actionButton(ns('cancel'), "Cancel", class = "btn-cancel right", 
                 icon("glyphicon glyphicon-remove", lib = "glyphicon")),
  )
}

#' modify_specimen Server Functions
#'
#' @noRd 
mod_modal_modifyValue_server <- function(id, dta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    submitted <- reactiveVal(0)
    close_form <- reactiveVal(0)
    
    input_width <- "100%"
    
    iv <- shinyvalidate::InputValidator$new()
    
    iv$add_rule("new_value", sv_required())
    
    output$info_ui <- renderUI({
      
      session$userData$db_trigger()
      
      what_lab <- col_labels[[id]]
      
      # get dta information from DB directly, in case this edit is 
      # asked repeatedly within the session
      
      # Need to find out if a change is asked in the specimen_info (that has lab_no)
      # if not, then it must be on the sample_info and look with the unique id
      # Do not use the unique id for the specimen info, as there might be more than one lab_no's
      # for one unique_id
      
      if( "lab_no" %in% names(dta())){
        
        dta <- dbase_specimen %>% tbl("specimen_info") %>% filter(lab_no == !!dta()$lab_no) %>% collect()
        
        lab_no <- dta$lab_no
        bococ <- get_fromDB(dbase_specimen, "sample_info", "bococ", dta()$unique_id)
        
      } else {
        
        dta <- dbase_specimen %>% tbl("sample_info") %>% filter(unique_id == !!dta()$unique_id) %>% collect()
        
        bococ <- dta$bococ
        
        lab_no <- paste(get_fromDB(dbase_specimen, "specimen_info", "lab_no", dta()$unique_id), collapse = ", ")
      }
      
      
      
      # Decide the input box type
      input_box <- NULL
      
      browser()
      if( id %in% names(col_values) ) {
        
        current_value <- dta[[id]]
        input_box <- selectInput(ns("new_value"), "Enter new value", choices = c("",  col_values[id]))
        
      }
      
      if( id %in% date_cols) {
        
        current_value <- to_date(dta[[id]]) %>% format("%d/%m/%Y")
        
        input_box <- dateInput(ns("new_value"), glue::glue("Enter new date"), to_date(dta[[id]]), format = "dd/mm/yyyy", width = input_width)
        
        iv$add_rule("new_value", ~valid_date(., col_labels[[id]]))
        
      }
      
      
      if( id %in% date_time_cols) {
        
        current_value <- to_date_time(dta[[id]]) %>% format("%d/%m/%Y %H:%M")
        
        input_box <- textInput(ns("new_value"), glue::glue("Enter new date-time (DD/MM/YYYY HH:MM)"), value = current_value, width = input_width)
        
        iv$add_rule("new_value", function(text){
          
          if(is.na(lubridate::dmy_hm(text))) {
            "Not a valid date time text. Please enter the date time in the form of 
            DD/MM/YYYY HH:MM"
          }
        })
        
      }
      
      if( !id %in% names(col_values) && !id %in% date_time_cols && !id %in% date_cols){
        
        current_value <- dta[[id]]
        
        if(inherits(dta[[id]], c("integer", "numeric"))){
          
          
          input_box <- numericInput(ns("new_value"), "New value", min = 0, value = current_value, width = input_width)
          iv$add_rule("new_value", sv_gte(0))
          
        } else { 
          
          input_box <- textInput(ns("new_value"), "Enter new value", value = current_value, width = input_width)
          
        }
        
      }
      
      if(id == "bococ"){
        iv$add_rule("new_value", valid_bococ)
      }
      
      
      # NEED to capture the input$new_value first before returning it
      # I need to process the value in case it is a date or date time
      # For date-time, I accept text. I need to transform it to numeric or Pisxct to save it in the DB
      
      # What happens with simple Date 
      
      tagList(
        p("Lab no: ", strong(lab_no)),
        p("BOCOC :", strong(bococ)),
        hr(),
        p("You are changing the ", code(what_lab)),
        p("Current value of ", code(what_lab), ": ", strong(current_value)),
        input_box
      )
      
      
    })
    
    observeEvent(input$cancel, {
      
      iv$disable() # so when the user enters again, it finds all the same 
      close_form(close_form()+1)
      
    })
    
    
    observeEvent(input$submit, {
      
      if (iv$is_valid()) {
        
        iv$disable()
        removeNotification("submit_message")
        
        #shinyjs::reset("form")
        submitted(submitted()+1)
        
      } else {
        iv$enable() # Start showing validation feedback
        
        showNotification(
          "Please correct the errors in the form and try again",
          id = "submit_message", type = "error")
      }
      
      
    })
    
    
    
    return(
      list(
        new_value = reactive(input$new_value),
        submit = submitted,
        cancel = close_form,
        id  = id # to know what changed
      )
    )
    
    
  })
}

## To be copied in the UI
# mod_modal_modifyValue_ui("modify_specimen_1")

## To be copied in the server
# mod_modal_modifyValue_server("modify_specimen_1")
