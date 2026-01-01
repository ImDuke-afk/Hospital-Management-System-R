library(shiny)
library(DBI)
library(RMySQL)
library(dplyr)
library(rmarkdown)

# Database connection
con <- dbConnect(
  MySQL(),
  user = "root",
  password = "",
  dbname = "HospitalDB",
  host = "localhost"
)

ui <- fluidPage(
  titlePanel("Hospital Patient PDF System"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("patient_id", "Select Patient:", choices = NULL),
      downloadButton("download_pdf", "Download Patient PDF")
    ),
    mainPanel(
      tableOutput("patient_table")
    )
  )
)

server <- function(input, output, session) {
  
  patients <- reactive({
    dbGetQuery(con, "
      SELECT 
        Patients.PatientID,
        Patients.FirstName,
        Patients.LastName,
        Patients.Age,
        Patients.Condition,
        Patients.AdmissionDate,
        Doctors.DoctorName,
        Doctors.Specialty,
        CASE
          WHEN Patients.Age >= 60 THEN 'High Risk'
          WHEN Patients.Condition IN ('Diabetes','Hypertension') THEN 'Medium Risk'
          ELSE 'Low Risk'
        END AS ConditionIdentifier
      FROM Patients
      INNER JOIN Doctors
      ON Patients.DoctorID = Doctors.DoctorID
    ")
  })
  
  observe({
    df <- patients()
    updateSelectInput(
      session,
      "patient_id",
      choices = setNames(df$PatientID,
                         paste(df$FirstName, df$LastName))
    )
  })
  
  output$patient_table <- renderTable({
    patients()
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("Patient_Report_", input$patient_id, ".pdf")
    },
    content = function(file) {
      patient <- patients() %>%
        filter(PatientID == input$patient_id)
      
      rmarkdown::render(
        input = "patient_report.Rmd",
        output_file = file,
        params = list(patient = patient),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)
