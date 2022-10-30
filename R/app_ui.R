#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1("BioTrack"),
      tabsetPanel(id = "tabs", type = "hidden",
                  
                  tabPanel("Initial",
                           actionButton("add_sample_info", "Add new Sample")
                           ),
                  
                  tabPanel("Add sample info",
                           fluidRow(
                             column(3),
                             col_6(
                               mod_sample_information_ui("sample_information_1")
                             ),
                             column(3)
                           )
                  ),
                  tabPanel("Specimen",
                           fluidRow(
                             column(4),
                             col_4(
                               mod_storage_information_ui("storage_information_1")
                             ),
                             column(4)
                           )
                           
                           )
      )
      
      
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BioTrack"
    ),
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    waiter::use_waiter()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
