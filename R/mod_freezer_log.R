#' freezer_log UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_freezer_log_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 10,
        downloadLink(ns("down_freezer_log"), "Download as .docx"),
        uiOutput(ns("spec_log"))
    )
  )
}

#' freezer_log Server Functions
#'
#' @noRd 
mod_freezer_log_server <- function(id, tbl_merged){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    freezer <- "-80"
    title <- glue::glue("{freezer}°C Freezer A Specimen Log")
    log_version <- "Version 3.0 April 2022"
    
    page_props <- officer::prop_section(page_size = officer::page_size(orient = "landscape"))
    
    output$down_freezer_log <- downloadHandler(
      
      filename = function(){
        glue::glue("Freezer_{freezer}C-{format(Sys.time(), '%d/%m/%Y %H:%M')} .docx")
      },
      
      content = function(file) {
        
        spec_log() %>% 
          flextable::add_footer_lines(glue::glue("Date exported: ", format(Sys.time(), "%d/%m/%Y %H:%M"))) %>% 
        flextable::save_as_docx(
          path = file,
          pr_section = page_props
        )
        
        
      }
      
      
    )
    
    
    
    output$spec_log <- renderUI({
      
      spec_log() %>%
        flextable::htmltools_value()
      
    })
    
    spec_log <- reactive({
      browser()
      table <- 
        tbl_merged() %>% 
        select(lab_no, freezer, box, rack, drawer,
               all_of(c("unique_id", "firstname", "surname", "date_collection"))) %>% 
        mutate(
          initials =  paste0(substr(firstname, 1, 1), substr(surname, 1, 1)),
          date_collection = format(date_collection, "%d/%m/%Y")
        ) %>% 
        tidyr::unite(content, c(lab_no, initials, date_collection), remove = TRUE,sep = "-") %>% 
        filter(freezer == !!freezer) %>% 
        select(rack, drawer, box, content) %>% 
        filter(!is.na(box)) %>% 
        tidyr::complete(rack = LETTERS[1:4], box = as.character(c(1:3)), drawer = as.character(c(1:5))) 
      
      rack_tbl <- table %>% 
        filter(rack == "A") %>% 
        select(-rack) %>% 
        rename (Box = box) %>% 
        tidyr::pivot_wider( 
          names_from = drawer, 
          values_from = content, 
          values_fn = ~glue::glue_collapse(., sep = "\n")
        ) 
      
      rack_tbl %>% 
        flextable::flextable() %>% 
        flextable::add_header_row(values = c("", "Drawers"), colwidths = c(1, 5)) %>% 
        flextable::theme_box() %>%
        flextable::set_caption(paste0(title, "\n")) %>% 
        flextable::set_table_properties("fixed") %>% 
        flextable::width(width = 1.80) %>% 
        flextable::fontsize(size = 9) %>% 
        flextable::add_footer_lines(glue::glue("BOCOC: {freezer} Freezer. Specimen Log {log_version}")) 
      
    })
    
    
    
    
  })
}

## To be copied in the UI
# mod_freezer_log_ui("freezer_log_1")

## To be copied in the server
# mod_freezer_log_server("freezer_log_1")
