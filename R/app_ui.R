#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage( skin = "purple",
      dashboardHeader(title =  p(icon("vial-circle-check"), " BioTrack" )),
      dashboardSidebar(
        sidebarMenu(id = "left_tabs",
                    menuItem("Add sample", tabName = "add_sample", icon = icon("plus"), selected =TRUE),
                    menuItem("View/Edit Specimem", tabName = "view", icon = icon("searchengin")),
                    menuItem("Specimen Logs", tabName = "tables", icon = icon("table")),
                    menuItem("Freezer Logs", tabName = "freezer_log", icon = icon("file")),
                    menuItem("Actions log", tabName = "log_file", icon = icon("file")),
                    tags$hr(style = "border-color: white;width: 80%"),
                    p(paste0("Version: ", golem::get_golem_version()), style = "margin-left:25px"),
                    div(textOutput("userName"), style = "margin-left:25px")
                    
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "add_sample",
                  tabsetPanel(id = "tabs", type = "hidden",
                              
                              # tabPanel("Initial",
                              #          actionButton("add_sample_info", "Add new Sample")
                              # ),
                              
                              tabPanel("Add sample info",
                                       fluidRow(
                                         column(3),
                                         col_6(
                                           box(width = NULL, title = h3("Sample Information Form"),
                                               mod_sample_information_ui("sample_information_1")
                                           )
                                         ),
                                         column(3)
                                       )
                              ),
                              tabPanel("Specimen",
                                       fluidRow(
                                         column(4),
                                         col_4(
                                           box(width = NULL, title = h3("Processing / Storage Information:", style = "text-align: center;"),
                                               mod_storage_information_ui("storage_information_1")
                                           )
                                         ),
                                         column(4)
                                       )
                                       
                              )
                  )
                  
                  
          ),
          tabItem(tabName = "view", 
                  mod_view_edit_specimen_ui("view_edit_specimen_1")
          ),
          tabItem(tabName = "tables", 
                  mod_tables_ui("tables_1") 
          ),
          tabItem(tabName = "freezer_log", 
                  mod_freezer_log_ui("freezer_log_1")
          ),
          tabItem(tabName = "log_file", 
                  mod_log_file_ui("log_file_1")
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
    shinyjs::extendShinyjs(text = jsResetCode, functions = "reset1"),
    waiter::use_waiter()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
