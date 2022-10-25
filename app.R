library(shiny)


sample_types <- c("Peripheral blood",
                  "Plasma", "Serum", "Urine", "Stools", "Bronchial aspirations"
                  )

ui <- fluidPage(
  
  h3("1.Contact Information:"),
  hr(),
  textInput("name", "Patient Name"),
  textInput("surname", "Patient Surname"),
  selectInput("gender", "Gender", c("Male", "Female", "Other")),
  numericInput("bococ", "BOCOC ID", value =NULL),
  dateInput("dob", "Date of birth"),
  textInput("nationality", "Nationality"),
  
  h3("2.Clinical Information:"),
  textInput("diagnosis", "Diagnosis:"),
  selectInput("status", "Status", c("Metastatic", "Non- metastatic")),
  textInput("doctor", "Referring doctor"),
  selectInput("signed", "Consent signed", c("Yes", "No")),
  
  h3("3.Sample Type received:"),
  splitLayout(
    selectInput("slct1", "1.", choices = sample_types),
    numericInput("ml", "ml", value = 0)
  )
  
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)