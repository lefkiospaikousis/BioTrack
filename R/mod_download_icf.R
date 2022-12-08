#' download_icf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_icf_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadLink(ns("download"), "Download ICF for selected Lab no(s)", class = "downExcel"),
    tags$head(tags$style(".downExcel{background-color:transparent;} .downExcel{color: #337ab7;}  .downExcel{border:0px;}
   .downExcel{outline:0px;} .downExcel{font-size:10px;"))
  )
}

#' download_icf Server Functions
#'
#' @noRd 
mod_download_icf_server <- function(id, files_download){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$download <- downloadHandler(
      
      filename = function() {
        
        if(!file.exists(files_download()$path_icf)){
          
          show_toast("error", "Something went wrong", "Could not find the ICF for this sample. Please contact support asap")
          return()
        }
        
        x <- try(files_download(), silent = TRUE)
        
        if( inherits(x, 'try-error') ){
          show_toast("error", "", "Select a Lab no fist and then click to download the ICF")
        }
        
        files <- strsplit(files_download()$path_icf, split = "\n") %>% unlist()
        
        
        if(length(files) > 1){
          
          return(
            #paste("ICF_MultipleFiles.zip", sep = "")
            paste0("ICF-", files_download()$lab_no, ".zip")
          )
        } 
        
        if(length(files) == 1){
          
          return(
            paste0("ICF-", files_download()$lab_no, "-", files ,".pdf")
          )
        } 
        
      },
      
      content = function(file) {
        
        id <- showNotification(
          "Downloading files...", 
          duration = NULL, 
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)
        
        files <- strsplit(files_download()$path_icf, split = "\n") %>% unlist()
        
        if(length(files) > 1){
          
          #files <- files_download()$path_icf
          
          # keep only the ones that exist. In case one of the files  is not there, the zip fails
          
          file_paths <- file.path("ICF", files_download()$unique_id, files)
          
          pdf_exist <- file.exists(file_paths) %>% setNames(file_paths) 
          
          
          if(!all(pdf_exist)) show_toast("warning", "Something is wrong!", "Some of the ICF's you have requested, do not exist",
                                         keepVisible = TRUE)
          
          pdf_to_download <- pdf_exist %>% purrr::keep(isTRUE) %>% names()
          
          zip::zip(
            zipfile = file,
            files = pdf_to_download,
            recurse = FALSE,
            mode = "cherry-pick"
          )
          
        } 
        
        if(length(files) == 1){
          
          path <- file.path("ICF", files_download()$unique_id, files)
          
          # keep only the ones that exist. In case one of the files  is not there, the zip fails
          pdf_exist <- file.exists(path) %>% setNames(path) 
          
          
          if(!file.exists(path)) {
            show_toast("warning", "Something is wrong!", 
                       "The ICF for this selection does not exist", keepVisible = TRUE)
            
          }
          
          
          file.copy(path, file, overwrite = TRUE )
          
          
        }
        
      } 
      
    )
    
    
  })
}

## To be copied in the UI
# mod_download_icf_ui("download_icf_1")

## To be copied in the server
# mod_download_icf_server("download_icf_1")
