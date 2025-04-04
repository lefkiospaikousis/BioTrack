#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  
  source(system.file("global.R", package = "BioTrack"))
  
  with_golem_options(
    app = shinyApp(
      ui =shinymanager::secure_app(app_ui, enable_admin = TRUE,
                                   tags_top = tags$div(
                                     tags$h3(icon("vial-circle-check"), " BioTrack", style = "align:center"),
                                     tags$img(
                                       src =  "www/bococ_image.jpg"  #"https://www.r-project.org/logo/Rlogo.png"
                                       , width = 100
                                     )
                                   )
                                   ),
      #ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
