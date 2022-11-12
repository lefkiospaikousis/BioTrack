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
    sidebarLayout(
      sidebarPanel(width = 2,
                   selectInput(ns("freezer"), "Freezer", choices = c("-80")),
                   selectInput(ns("rack"),"Rack", choices = c("A", "B", "C", "D")),
                   downloadLink(ns("down_freezer_log"), "Download Log as .docx")
      ),
      mainPanel(width = 8,
                box(width = NULL,
                    uiOutput(ns("spec_log"))
                )
      )
    )
    
  )
}

#' freezer_log Server Functions
#'
#' @noRd 
mod_freezer_log_server <- function(id, tbl_merged){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #freezer <- "-80"
    
    log_version <- get_golem_config("log_version") #  "Version 3.0 April 2022"
    
    page_props <- officer::prop_section(page_size = officer::page_size(orient = "landscape"))
    
    output$down_freezer_log <- downloadHandler(
      
      filename = function(){
        glue::glue("Freezer_{input$freezer}C-Rack_{input$rack}-{format(Sys.time(), '%d/%m/%Y_%H:%M')}.docx")
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
      
      title <- glue::glue("{input$freezer}°C Freezer - Rack {input$rack} - Specimen Log")
      
      #browser()
      rack_tbl <- tbl_merged() %>% 
        select(lab_no, freezer, box, rack, drawer, unique_id) %>% 
        # all_of(c("unique_id", "firstname", "surname", "date_collection"))) %>% 
        # mutate(
        #   initials =  paste0(substr(firstname, 1, 1), substr(surname, 1, 1)),
        #   date_collection = format(date_collection, "%d/%m/%Y")
        # ) %>% 
        #tidyr::unite(content, c(lab_no, initials, date_collection), remove = TRUE,sep = "-") %>% 
        filter(freezer == !!input$freezer, rack == !!input$rack) %>% 
        select(drawer, box, lab_no) %>% 
        #filter(!is.na(box)) %>% 
        tidyr::complete(box = as.character(c(1:3)), drawer = as.character(c(1:5))) %>%  #rack = LETTERS[1:4], 
        rename (Box = box) %>% 
        tidyr::pivot_wider( 
          names_from = drawer, 
          values_from = lab_no, 
          values_fn = ~glue::glue_collapse(., sep = "\n")
        ) 
     
      rack_tbl %>% 
        flextable::flextable() %>% 
        flextable::add_header_row(values = c(paste0("RACK ", input$rack), "Drawers"), colwidths = c(1, 5)) %>% 
        flextable::theme_box() %>%
        flextable::set_caption(
          flextable::as_paragraph(flextable::as_chunk(title, 
                                props = officer::fp_text(bold = TRUE, font.size = 14))),
          fp_p = officer::fp_par(padding.bottom = 10)
                               ) %>% 
        flextable::set_table_properties("fixed") %>% 
        flextable::width( j = c(2:6), width = 1.80) %>% 
        flextable::fontsize(size = 9) %>% 
        flextable::add_footer_lines(glue::glue("BOCOC: {input$freezer} Freezer. Rack {input$rack}. Specimen {log_version}")) 
      
    })
    
    
    
    
  })
}

## To be copied in the UI
# mod_freezer_log_ui("freezer_log_1")

## To be copied in the server
# mod_freezer_log_server("freezer_log_1")
