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
                   selectInput(ns("freezer"), "Freezer", choices = c("-80", "-20")),
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
    
    log_version <- get_golem_config("log_version") #  "Version 3.0 April 2022"
    
    page_props <- officer::prop_section(page_size = officer::page_size(orient = "landscape"))
    
    rv <- reactiveValues(rack_text = NULL)
    
    observe({
      req(input$freezer)
      shinyjs::toggleState("rack", condition = input$freezer != "-20")
      
      rv$rack_text <- switch (input$freezer,
                              "-80" = paste0("RACK ", input$rack),
                              # other freezers do not have racks
                              ""
      )
      
    })
    
    output$down_freezer_log <- downloadHandler(
      
      filename = function(){
        glue::glue("Freezer_{input$freezer}C-{rv$rack_text}-{format(Sys.time(), '%d/%m/%Y-%H:%M')}.docx")
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
      
      title <- glue::glue("{input$freezer}°C Freezer - {rv$rack_text} - Specimen Log")
      
      
      rack_tbl <- tbl_merged() %>% 
        select(lab_no, freezer, box, rack, drawer, unique_id) %>% 
        filter(freezer == !!input$freezer) %>% 
        {
          if(input$freezer == "-80"){
            filter(., rack == !!input$rack)
          } else {
            .
          }
        } %>% 
        select(drawer, box, lab_no) %>% 
        tidyr::complete(box = as.character(c(1:3)), drawer = as.character(c(1:5))) %>%  
        rename (Box = box) %>% 
        tidyr::pivot_wider( 
          names_from = drawer, 
          values_from = lab_no, 
          values_fn = ~glue::glue_collapse(., sep = "\n")
        ) 
      
      
      # corner_text <- switch (input$freezer,
      #                        "-80" = paste0("RACK ", input$rack),
      #                        ""
      # )
      
      rack_tbl %>% 
        flextable::flextable() %>% 
        flextable::add_header_row(values = c(rv$rack_text, "Drawers"), colwidths = c(1, 5)) %>% 
        flextable::theme_box() %>%
        flextable::set_caption(
          flextable::as_paragraph(flextable::as_chunk(title, 
                                                      props = officer::fp_text(bold = TRUE, font.size = 14))),
          fp_p = officer::fp_par(padding.bottom = 10)
        ) %>% 
        flextable::set_table_properties("fixed") %>% 
        flextable::width( j = c(2:6), width = 1.80) %>% 
        flextable::fontsize(size = 9) %>% 
        flextable::add_footer_lines(glue::glue("BOCOC: {input$freezer}°C Freezer. {rv$rack_text}. Specimen {log_version}")) 
      
    })
    
    
    
    
  })
}

## To be copied in the UI
# mod_freezer_log_ui("freezer_log_1")

## To be copied in the server
# mod_freezer_log_server("freezer_log_1")
