#' modify_specimen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_modify_specimen_ui <- function(id){
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
mod_modify_specimen_server <- function(id, specimen){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    submitted <- reactiveVal(0)
    close_form <- reactiveVal(0)
    
    
    iv <- shinyvalidate::InputValidator$new()
    
    iv$add_rule("new_value", sv_required())
    
    output$info_ui <- renderUI({

      if(id == "n_tubes") what_lab = "Number of tubes" else what_lab <- id

      x <- NULL


      if(inherits(specimen()[[id]], c("integer", "numeric"))){
        
        x <- numericInput(ns("new_value"), "New value", min = 0, value = specimen()[[id]], width = "100%")
        iv$add_rule("new_value", sv_gte(0))
        
      } else {
        
        x <- textInput(ns("new_value"), "New value", value = specimen()[[id]], width = "100%")
        
      }


      tagList(
        p("Lab no: ", strong(specimen()$lab_no)),
        p("You are changing the ", code(what_lab)),
        p("Current value of ", code(what_lab), ": ", strong(specimen()[[id]])),
        x
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
# mod_modify_specimen_ui("modify_specimen_1")

## To be copied in the server
# mod_modify_specimen_server("modify_specimen_1")
