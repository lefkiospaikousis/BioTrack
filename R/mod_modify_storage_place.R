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
      
      #"n_tubes",
      #"comment_place"
    )
    
    
    # Validation ----
    
    iv <- shinyvalidate::InputValidator$new()
    
    iv$add_rule("freezer", sv_required())
    
    iv$add_rule("drawer", sv_required())
    #iv$add_rule("n_tubes", sv_gt(0))
    
    iv_freezer <- shinyvalidate::InputValidator$new()
    iv_freezer$condition(~ input$freezer == "-80\u00B0C")
    
    iv_freezer$add_rule("rack", sv_required())
    iv_freezer$add_rule("box", sv_required())
    
    iv$add_validator(iv_freezer)
    
    
    output$info_ui <- renderUI({
      
      # reget specimen in case the detials were changed just now
      specimen <- get_specimen(dbase_specimen, specimen()$lab_no)
      session$userData$db_trigger()
      
      tagList(
        div(
          
          p("Lab no: ", strong(specimen$lab_no)),
          hr(),
          p("You are changing the Storage Place of the Specimen"),
          p("Current Storage Place: ", strong(specimen$place))
        ),
        div(style = "font-size:13px",
            #hr(style = "width: 80%"),
            h4("Storage place"),
            tags$table(
              
              tags$tr(width = "100%",
                      tags$td(width = "30%", div(class = "input-label",style = "", "Freezer:")),
                      tags$td(width = "70%", prettyRadioButtons(ns("freezer"), NULL, c("-80\u00B0C", "-20\u00B0C", "+4\u00B0C"), 
                                                                inline = TRUE,  fill = TRUE, selected = specimen$freezer, width = "100%"))),
              
              tags$tr(width = "100%",
                      tags$td(width = "30%", div(class = "input-label", "Rack:")),
                      tags$td(width = "70%", 
                              prettyRadioButtons(ns("rack"), NULL, c("A", "B", "C", "D"), 
                                                 fill = TRUE, inline = TRUE, selected = specimen$rack, width = "100%")
                      )),
              
              tags$tr(width = "100%",
                      tags$td(width = "30%", div(class = "input-label", "Drawer:")),
                      tags$td(width = "70%", prettyRadioButtons(ns("drawer"), NULL, c(1:5), 
                                                                inline = TRUE,  fill = TRUE, selected = specimen$drawer, width = "100%"))),
              
              tags$tr(width = "100%",
                      tags$td(width = "30%", div(class = "input-label", "Box:")),
                      tags$td(width = "70%",  
                              prettyRadioButtons(ns("box"), NULL, c(1:3), 
                                                 inline = TRUE,  fill = TRUE, selected = specimen$box, width = "100%")
                      )),
              
            )
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
    
    
    observeEvent(input$freezer,{
      
      shinyjs::toggleState("rack",  condition = input$freezer == "-80\u00B0C")
      shinyjs::toggleState("box",  condition = input$freezer == "-80\u00B0C")
    }, ignoreInit = TRUE)
    
    
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
