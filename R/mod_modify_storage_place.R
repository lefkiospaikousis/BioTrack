#' modify_storage_place UI Function
#'
#' @description A shiny Module that modifies only the Storage Place i.e. Freezer, Rack, Box, Drawer
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_modify_storage_place_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("info_ui")),
    hr(),
    actionButton(ns("submit"), "Done", class = "btn-submit",
                 icon("glyphicon glyphicon-ok", lib = "glyphicon")),
    actionButton(ns('cancel'), "Cancel", class = "btn-cancel right", 
                 icon("glyphicon glyphicon-remove", lib = "glyphicon")),
    hr()
    
  )
}

#' modify_storage_place Server Functions
#'
#' @noRd 
mod_modify_storage_place_server <- function(id, specimen){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    input_width <- "100%"
    
    submitted <- reactiveVal(0)
    close_form <- reactiveVal(0)
    
    all_fields <- c(
      "freezer",
      "rack",
      "drawer",
      "box"
    )
    
    
    # Validation ----
    
    iv <- shinyvalidate::InputValidator$new()
    
    iv$add_rule("freezer", sv_required())
    
    # Drawer -80 validation
    iv_freezer80 <- shinyvalidate::InputValidator$new()
    iv_freezer80$condition(~ input$freezer  %in% freezers_80 )
    
    iv_freezer80$add_rule("rack", sv_required())
    iv_freezer80$add_rule("box", sv_required())
    iv_freezer80$add_rule("drawer", sv_required())
    
    iv$add_validator(iv_freezer80)
    
    # Drawer -20 validation
    iv_freezer20 <- shinyvalidate::InputValidator$new()
    iv_freezer20$condition(~ input$freezer == freezer_20 )
    
    iv_freezer20$add_rule("drawer", sv_required())
    
    iv$add_validator(iv_freezer20)
    
    # Freezers ----
    
    observeEvent(input$freezer, {
      
      req(input$freezer)
      
      if(input$freezer == freezer_80_small){
        
        updateSelectInput(session, "rack", choices = LETTERS[1:4])
        updatePrettyRadioButtons(session, "drawer", choices = c(1:5), inline = TRUE)
        updatePrettyRadioButtons(session, "box", choices = c(1:3), inline = TRUE)
        
      }
      
      if(input$freezer == freezer_80_big){
        
        updateSelectInput(session, "rack", choices = LETTERS[1:19])
        updatePrettyRadioButtons(session, "drawer", choices = c(1:6), inline = TRUE)
        updatePrettyRadioButtons(session, "box", choices = c(1:5), inline = TRUE)
        
      }
      
      if(input$freezer == freezer_20){
        
        updateSelectInput(session, "rack", selected = character(0))
        updatePrettyRadioButtons(session, "drawer", choices = c(1:5), inline = TRUE)
        updatePrettyRadioButtons(session, "box", selected = character(0), inline = TRUE)
      }
      
      if(input$freezer == freezer_04){
        
        updateSelectInput(session, "rack", selected = character(0))
        updatePrettyRadioButtons(session, "box", selected = character(0), inline = TRUE)
        updatePrettyRadioButtons(session, "drawer", selected = character(0), inline = TRUE)
        
      }
      
      shinyjs::toggleState("rack",  condition = input$freezer %in% freezers_80 )
      shinyjs::toggleState("box",  condition = input$freezer %in% freezers_80 )
      
    },ignoreInit = TRUE)
    
    
    output$info_ui <- renderUI({
      
      # re-fetch specimen in case the details were changed just now
      specimen <- get_specimen(dbase_specimen, specimen()$lab_no)
      session$userData$db_trigger()
      
      tagList(
        div(
          p("Lab no: ", strong(specimen$lab_no)),
          hr(),
          p("You are changing the Storage Place of the Specimen"),
          p("Current Storage Place: ", strong(specimen$freezer), " at " , strong(specimen$place))
        ),
        div(
          storage_placeUI(ns), style = "font-size:13px",
        )
      )
      
    })
    
    # Data collection ----
    
    form_data <- reactive({
      
      # Temporary collection of the data 
      
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
        
        showNotification(
          "Please correct the errors in the form and try again",
          id = "submit_message", type = "error")
      }
      
      
    })
    
    
    #Return ####
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
# mod_modify_storage_place_ui("modify_storage_place_1")

## To be copied in the server
# mod_modify_storage_place_server("modify_storage_place_1")
