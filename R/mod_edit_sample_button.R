#' edit_sample_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_edit_sample_button_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("edit"), "Edit", icon("pen-to-square"), class = "btn_edit")
  )
}
    
#' edit_sample_button Server Functions
#'
#' @noRd 
mod_edit_sample_button_server <- function(id, sample_info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
      
    observeEvent(input$edit, {
      
      ids_allowed_all <- c("n_tubes", "status")
      
      if(isTRUE(as.logical(session$userData$user_info$admin)) | id %in% ids_allowed_all) {
        
        showModal(
          modalDialog(
            title = "Editing Sample Information data",
            size = "s", footer = NULL,
            mod_modal_modifyValue_ui(ns(id))
          )
        )
        
      } else {
        
        show_toast("error", "You are not authorised to change this field",
                   "An administrator can change this field")
      }
      
      
    })
    
    res <- mod_modal_modifyValue_server(id, sample_info)
    
    
    observeEvent(res$cancel(), {
      
      removeModal()
      
    }, ignoreInit = TRUE)
    
    observeEvent(res$submit(), {
      
      new_value <- res$new_value()
      col <- res$id
      
      unique_id <- sample_info()$unique_id
      
      x <- glue::glue_sql("UPDATE sample_info SET {col} = {new_value} WHERE unique_id = {unique_id}", .con = dbase_specimen)
      
      rs <- DBI::dbExecute(dbase_specimen, x)
      
      if(rs == 1){ 
        
        cat("Updated ", col, "for ", rs, " sample_info with unique_id: ", unique_id, "\n")
        
      } else {
        
        cat("Failed to update the ", col, " sample_info with unique_id: ", unique_id, "\n")
        
      }
      
      if(!golem::app_prod()) showNotification(
        glue::glue("Updated sample_info ", unique_id, " - changed {col} to {new_value}")
      )
      
      removeModal()
      
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      show_toast("success", "", "Successful change")
      
      #bococ <- dbase_specimen %>% tbl("sample_info") %>% filter(unique_id == !!sample_info()$unique_id) %>% pull(bococ)
      
      # Add to log
      try({add_to_logFile("Modified Sample Information data", session$userData$user, 
                          info = list(lab_no = "", #sample_info()$lab_no,
                                      bococ = sample_info()$bococ,
                                      col = col,
                                      old_value = sample_info()[[col]],
                                      new_value = new_value
                                      
                          ))}, silent = TRUE)
      
      
    }, ignoreInit = TRUE)
    
  })
}
    
## To be copied in the UI
# mod_edit_sample_button_ui("edit_sample_button_1")
    
## To be copied in the server
# mod_edit_sample_button_server("edit_sample_button_1")
