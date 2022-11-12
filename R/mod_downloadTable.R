#' downloadTable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_downloadTable_ui <- function(id, label = "Download as .xlsx"){
  ns <- NS(id)
  tagList(
    downloadLink(ns("downloadTable"), label = label, class = "downExcel"),
    tags$head(tags$style(".downExcel{background-color:transparent;} .downExcel{color: #337ab7;}  .downExcel{border:0px;}
   .downExcel{outline:0px;} .downExcel{font-size:10px;"))
  )
}
    
#' downloadTable Server Functions
#' @param table_name String. A name to be used in the filename when downloading
#' @param the_table A reactive element that leads to a datframe. Put it without the ()
#' @details This function saves only plots. And specifically reactive plots. Havent tested it for 
#' reactivevalues. Note the () I use in the server part. Not sure why I need this.
#'
#' @noRd 
mod_downloadTable_server <- function(id, table_name, the_table){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$downloadTable <- downloadHandler(
      
      filename = function() {
        paste(table_name,".xlsx", sep="")
      },
      
      # See here that i wrapped the `the_plot` with `()`
      content = function(file) {
        writexl::write_xlsx(path = file, x = the_table())
      }
    )
  })
}
    
## To be copied in the UI
# mod_downloadTable_ui("downloadTable_ui_1")
    
## To be copied in the server
# mod_downloadTable_server("downloadTable_ui_1")
