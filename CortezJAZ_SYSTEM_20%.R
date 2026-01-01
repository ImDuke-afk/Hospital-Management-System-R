library(shiny)
library(shinydashboard)
library(DBI)
library(RMySQL)
library(dplyr)
library(DT)
library(rmarkdown)
library(shinyjs) 

# -------------------------------
# 1. DATABASE CONNECTION
# -------------------------------
connectDB <- function() {
  dbConnect(
    MySQL(),
    user = "root",
    password = "",
    dbname = "HospitalDB",
    host = "localhost"
  )
}

# -------------------------------
# 2. HELPER: ENSURE SIGNATURE FOLDER EXISTS
# -------------------------------
if (!dir.exists("signatures")) {
  dir.create("signatures")
}

# -------------------------------
# 3. ROOM LOGIC
# -------------------------------
floors <- 1:8
rooms  <- 1:12
valid_rooms <- as.vector(outer(rooms, floors, function(r, f) f * 100 + r))
valid_rooms <- sort(valid_rooms)

# -------------------------------
# 4. UI
# -------------------------------
ui <- fluidPage(
  useShinyjs(),
  title = "MediCare System",
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap"),
    tags$style(HTML("
      body, h1, h2, h3, h4, h5, h6, .main-header .logo { font-family: 'Roboto', sans-serif !important; }
      .login-bg { background: linear-gradient(135deg, #006064 0%, #4dd0e1 100%); height: 100vh; display: flex; align-items: center; justify-content: center; }
      .login-card { background: rgba(255, 255, 255, 0.95); padding: 40px; width: 400px; border-radius: 15px; box-shadow: 0 20px 40px rgba(0,0,0,0.2); text-align: center; }
      .forgot-link { color: #006064; cursor: pointer; margin-top: 15px; display: block; text-decoration: underline; font-size: 0.9em; }
      .content-wrapper, .right-side { background-color: #cfd8dc !important; }
      .skin-blue .main-header .navbar { background-color: #37474f !important; }
      .skin-blue .main-header .logo { background-color: #263238 !important; }
      .skin-blue .main-sidebar { background-color: #263238 !important; }
      .small-box { cursor: pointer; border-radius: 8px; transition: transform 0.2s; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
      .small-box:hover { transform: scale(1.02); }
      .box { border-top: 3px solid #00acc1; border-radius: 10px; background: #fff; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
      .btn { border-radius: 25px; text-transform: uppercase; font-weight: bold; }
    "))
  ),
  uiOutput("page_content")
)

# -------------------------------
# 5. SERVER
# -------------------------------
server <- function(input, output, session) {
  
  user_session <- reactiveValues(logged_in = FALSE, user_name = NULL)
  dash_state   <- reactiveValues(view = "high_risk")
  
  con <- connectDB()
  onStop(function() { dbDisconnect(con) })
  
  # --- UI RENDER ---
  output$page_content <- renderUI({
    if (user_session$logged_in == FALSE) {
      div(class = "login-bg",
          div(class = "login-card",
              img(src = "https://cdn-icons-png.flaticon.com/512/3063/3063176.png", width="60px", style="margin-bottom:10px;"),
              h2(tagList(icon("hospital-user"), " MediCare Admin"), style="color:#006064; font-weight:bold; margin-bottom:20px;"),
              textInput("user_name", label=NULL, placeholder="Username", width="100%"),
              passwordInput("user_pass", label=NULL, placeholder="Password", width="100%"),
              br(),
              actionButton("login_btn", tagList(icon("sign-in-alt"), " Secure Login"), class="btn-info btn-lg btn-block", style="background-color:#00838f; border:none;"),
              a(id = "forgot_link", tagList(icon("key"), " Forgot Password?"), class="forgot-link", onclick = "Shiny.setInputValue('forgot_click', 1, {priority: 'event'})"),
              hidden(div(id="login_error", style="color: red; margin-top:10px;", icon("exclamation-circle"), " Invalid Credentials"))
          )
      )
    } else {
      dashboardPage(
        skin = "blue",
        dashboardHeader(title = "MediCare System", tags$li(class="dropdown", tags$a(id="logout_btn", href="#", class="action-button", icon("sign-out-alt"), "Logout"))),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Dashboard", tabName="dashboard", icon=icon("chart-line")),
            menuItem("Manage Patients", tabName="crud", icon=icon("user-injured")),
            menuItem("Manage Doctors", tabName="doctors", icon=icon("user-md")),
            menuItem("Reports", tabName="reports", icon=icon("file-medical-alt")),
            menuItem("User Admin", tabName="users", icon=icon("users-cog"))
          )
        ),
        dashboardBody(
          tabItems(
            # TAB 1: DASHBOARD
            tabItem(tabName="dashboard",
                    fluidRow(
                      div(id="clk_total", valueBoxOutput("box_total", width=4)),
                      div(id="clk_risk", valueBoxOutput("box_high_risk", width=4)),
                      div(id="clk_doc", valueBoxOutput("box_doctors", width=4))
                    ),
                    fluidRow(box(width=12, title=textOutput("dash_table_title"), status="primary", solidHeader=TRUE, DTOutput("dash_table_content")))
            ),
            # TAB 2: PATIENTS
            tabItem(tabName="crud",
                    fluidRow(
                      box(width=4, title=tagList(icon("edit"), " Patient Form"), status="primary", solidHeader=TRUE,
                          textInput("first_name", tagList(icon("user"), "First Name")),
                          textInput("last_name", tagList(icon("user"), "Last Name")),
                          fluidRow(
                            column(6, numericInput("age", tagList(icon("birthday-cake"),"Age"), value=30)),
                            column(6, dateInput("adm_date", tagList(icon("calendar-alt"),"Date"), value=Sys.Date()))
                          ),
                          textInput("condition", tagList(icon("notes-medical"), "Condition")),
                          selectInput("doctor_id", tagList(icon("user-md"), "Assign Doctor"), choices=NULL),
                          hr(),
                          actionButton("add_patient", "Add", icon=icon("plus"), class="btn-success"),
                          actionButton("update_patient", "Edit", icon=icon("pen"), class="btn-warning"),
                          actionButton("delete_patient", "Del", icon=icon("trash"), class="btn-danger")
                      ),
                      box(width=8, title="Records", status="primary", DTOutput("patient_table_crud"))
                    )
            ),
            # TAB 3: DOCTORS
            tabItem(tabName="doctors",
                    fluidRow(
                      box(width=4, title=tagList(icon("user-md"), " Doctor Form"), status="primary", solidHeader=TRUE,
                          textInput("doc_name", tagList(icon("signature"), "Doctor Name")),
                          textInput("doc_spec", tagList(icon("briefcase-medical"), "Specialty")),
                          fluidRow(
                            column(6, selectInput("doc_room", tagList(icon("door-open"), "Room #"), choices=valid_rooms)),
                            column(6, textInput("doc_contact", tagList(icon("phone"), "Contact")))
                          ),
                          textInput("doc_sched", tagList(icon("clock"), "Schedule")),
                          fileInput("doc_sig", label = tagList(icon("file-signature"), "Upload Signature (PNG/JPG)"), accept = c("image/png", "image/jpeg")),
                          hr(),
                          actionButton("add_doc", "Add Doctor", icon=icon("plus"), class="btn-success"),
                          actionButton("update_doc", "Update", icon=icon("pen"), class="btn-warning"),
                          actionButton("delete_doc", "Delete", icon=icon("trash"), class="btn-danger")
                      ),
                      box(width=8, title="Doctor List", status="primary", DTOutput("doctor_table"))
                    )
            ),
            # TAB 4 & 5
            tabItem(tabName="reports", fluidRow(box(width=12, title="PDF", status="info", solidHeader=TRUE, DTOutput("patient_table_pdf"), br(), downloadButton("download_pdf", "Download PDF", class="btn-primary")))),
            tabItem(tabName="users", fluidRow(box(width=4, title="Add Admin", status="warning", solidHeader=TRUE, textInput("new_user", "Username"), passwordInput("new_pass", "Password"), textInput("sec_q", "Security Question"), textInput("sec_a", "Answer"), br(), actionButton("add_admin_btn", "Create", icon=icon("check"), class="btn-success")), box(width=8, title="Users", status="warning", DTOutput("admin_table"))))
          )
        )
      )
    }
  })
  
  # --- LOGIN LOGIC ---
  observeEvent(input$login_btn, {
    req(input$user_name, input$user_pass)
    sql <- sprintf("SELECT * FROM Users WHERE Username = '%s' AND Password = '%s'", input$user_name, input$user_pass)
    res <- dbGetQuery(con, sql)
    if (nrow(res) > 0) { user_session$logged_in <- TRUE } else { shinyjs::show("login_error") }
  })
  
  # --- LOGOUT LOGIC (MODIFIED WITH CONFIRMATION) ---
  observeEvent(input$logout_btn, {
    showModal(modalDialog(
      title = tagList(icon("sign-out-alt"), " Confirm Exit"),
      div(style="text-align: center; padding: 20px;",
          h4("Are you sure you want to exit?"),
          p("Please type ", tags$b("CONFIRM", style="color: red;"), " to proceed."),
          textInput("logout_confirmation_text", label = NULL, placeholder = "Type CONFIRM")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_logout_final", "Log Out", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_logout_final, {
    req(input$logout_confirmation_text)
    # Check if text matches CONFIRM (Case Insensitive)
    if (toupper(input$logout_confirmation_text) == "CONFIRM") {
      removeModal()
      user_session$logged_in <- FALSE
      user_session$user_name <- NULL
      showNotification("Logged out successfully.", type = "message")
    } else {
      showNotification("Incorrect confirmation text. Please type CONFIRM.", type = "error")
    }
  })
  
  # --- FORGOT PASSWORD ---
  observeEvent(input$forgot_click, {
    showModal(modalDialog(
      title = tagList(icon("unlock-alt"), " Reset Password"),
      textInput("reset_user", "Enter Username"),
      actionButton("check_user_btn", "Verify User", icon=icon("search")),
      uiOutput("reset_ui"),
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$check_user_btn, {
    req(input$reset_user)
    sql <- sprintf("SELECT SecurityQuestion FROM Users WHERE Username = '%s'", input$reset_user)
    res <- dbGetQuery(con, sql)
    if(nrow(res) > 0) {
      output$reset_ui <- renderUI({ tagList(hr(), h4(icon("question"), " Security Question:"), h5(style="font-weight:bold; color:#006064", res$SecurityQuestion[1]), textInput("reset_answer", "Your Answer"), passwordInput("reset_new_pass", "New Password"), actionButton("confirm_reset_btn", "Reset Password", class="btn-danger", icon=icon("sync"))) })
    } else { showNotification("User not found!", type="error") }
  })
  observeEvent(input$confirm_reset_btn, {
    req(input$reset_answer, input$reset_new_pass)
    sql <- sprintf("SELECT * FROM Users WHERE Username = '%s' AND SecurityAnswer = '%s'", input$reset_user, input$reset_answer)
    res <- dbGetQuery(con, sql)
    if(nrow(res) > 0) {
      dbExecute(con, sprintf("UPDATE Users SET Password = '%s' WHERE Username = '%s'", input$reset_new_pass, input$reset_user))
      removeModal(); showNotification("Password Reset Successfully!", type="message")
    } else { showNotification("Incorrect Answer!", type="error") }
  })
  
  # --- DATA FETCHING ---
  patients_data <- reactive({ input$add_patient; input$update_patient; input$delete_patient; dbGetQuery(con, "SELECT p.PatientID, p.FirstName, p.LastName, p.Age, p.Condition, p.AdmissionDate, p.DoctorID, d.DoctorName FROM Patients p LEFT JOIN Doctors d ON p.DoctorID = d.DoctorID") })
  doctors_data  <- reactive({ input$add_doc; input$update_doc; input$delete_doc; dbGetQuery(con, "SELECT * FROM Doctors") })
  users_data    <- reactive({ input$add_admin_btn; dbGetQuery(con, "SELECT UserID, Username, SecurityQuestion FROM Users") })
  
  observe({ req(user_session$logged_in); docs <- doctors_data(); updateSelectInput(session, "doctor_id", choices = c("Select"="", setNames(docs$DoctorID, docs$DoctorName))) })
  
  # --- DASHBOARD ---
  onclick("clk_total", { dash_state$view <- "all" })
  onclick("clk_risk",  { dash_state$view <- "high_risk" })
  onclick("clk_doc",   { dash_state$view <- "doctors" })
  output$box_total <- renderValueBox({ valueBox(nrow(patients_data()), "Total Patients", icon=icon("users"), color="aqua") })
  output$box_high_risk <- renderValueBox({ valueBox(sum(patients_data()$Age >= 60), "High Risk", icon=icon("heartbeat"), color="red") })
  output$box_doctors <- renderValueBox({ valueBox(nrow(doctors_data()), "Active Doctors", icon=icon("user-md"), color="green") })
  output$dash_table_title <- renderText({ if(dash_state$view == "all") "All Patients" else if(dash_state$view == "high_risk") "High Risk Patients" else "Doctor Workload" })
  output$dash_table_content <- renderDT({
    data <- patients_data(); if(dash_state$view == "all") datatable(data, options=list(pageLength=5))
    else if(dash_state$view == "high_risk") datatable(data[data$Age >= 60, ], options=list(pageLength=5))
    else { stats <- data %>% group_by(DoctorName) %>% summarise(Count=n()); datatable(stats, options=list(dom='t')) }
  })
  
  # --- PATIENT CRUD ---
  output$patient_table_crud <- renderDT({ datatable(patients_data(), selection='single', options=list(pageLength=5, scrollX=T)) })
  observe({ req(input$patient_table_crud_rows_selected); d <- patients_data()[input$patient_table_crud_rows_selected, ]; updateTextInput(session, "first_name", value=d$FirstName); updateTextInput(session, "last_name", value=d$LastName); updateNumericInput(session, "age", value=d$Age); updateTextInput(session, "condition", value=d$Condition); updateDateInput(session, "adm_date", value=d$AdmissionDate); updateSelectInput(session, "doctor_id", selected=d$DoctorID) })
  
  observeEvent(input$add_patient, {
    req(input$first_name, input$doctor_id)
    sql <- sprintf("INSERT INTO Patients (FirstName, LastName, Age, `Condition`, AdmissionDate, DoctorID) VALUES ('%s', '%s', %d, '%s', '%s', %d)", input$first_name, input$last_name, input$age, input$condition, as.character(input$adm_date), as.integer(input$doctor_id))
    dbExecute(con, sql); showNotification("Patient Added", type="message")
  })
  observeEvent(input$update_patient, {
    req(input$patient_table_crud_rows_selected)
    id <- patients_data()[input$patient_table_crud_rows_selected, "PatientID"]
    sql <- sprintf("UPDATE Patients SET FirstName='%s', LastName='%s', Age=%d, `Condition`='%s', AdmissionDate='%s', DoctorID=%d WHERE PatientID=%d", input$first_name, input$last_name, input$age, input$condition, as.character(input$adm_date), as.integer(input$doctor_id), id)
    dbExecute(con, sql); showNotification("Patient Updated", type="warning")
  })
  observeEvent(input$delete_patient, {
    req(input$patient_table_crud_rows_selected)
    id <- patients_data()[input$patient_table_crud_rows_selected, "PatientID"]
    dbExecute(con, sprintf("DELETE FROM Patients WHERE PatientID=%d", id)); showNotification("Patient Deleted", type="error")
  })
  
  # --- DOCTOR CRUD ---
  output$doctor_table <- renderDT({ datatable(doctors_data(), selection='single', options=list(pageLength=5, scrollX=T)) })
  observe({ req(input$doctor_table_rows_selected); d <- doctors_data()[input$doctor_table_rows_selected, ]; updateTextInput(session, "doc_name", value=d$DoctorName); updateTextInput(session, "doc_spec", value=d$Specialty); updateSelectInput(session, "doc_room", selected=d$RoomNumber); updateTextInput(session, "doc_contact", value=d$ContactNumber); updateTextInput(session, "doc_sched", value=d$Schedule) })
  
  observeEvent(input$add_doc, {
    req(input$doc_name, input$doc_spec)
    tryCatch({
      sql <- sprintf("INSERT INTO Doctors (DoctorName, Specialty, RoomNumber, ContactNumber, Schedule) VALUES ('%s', '%s', %d, '%s', '%s')", 
                     input$doc_name, input$doc_spec, as.integer(input$doc_room), input$doc_contact, input$doc_sched)
      dbExecute(con, sql)
      if (!is.null(input$doc_sig)) {
        new_id_df <- dbGetQuery(con, "SELECT MAX(DoctorID) as id FROM Doctors")
        new_id <- new_id_df$id[1]
        file.copy(input$doc_sig$datapath, paste0("signatures/", new_id, ".png"), overwrite = TRUE)
      }
      showNotification("Doctor Added!", type="message")
    }, error = function(e) { showNotification(paste("Error:", e$message), type="error") })
  })
  
  observeEvent(input$update_doc, {
    req(input$doctor_table_rows_selected)
    id <- doctors_data()[input$doctor_table_rows_selected, "DoctorID"]
    tryCatch({
      sql <- sprintf("UPDATE Doctors SET DoctorName='%s', Specialty='%s', RoomNumber=%d, ContactNumber='%s', Schedule='%s' WHERE DoctorID=%d", 
                     input$doc_name, input$doc_spec, as.integer(input$doc_room), input$doc_contact, input$doc_sched, id)
      dbExecute(con, sql)
      if (!is.null(input$doc_sig)) {
        file.copy(input$doc_sig$datapath, paste0("signatures/", id, ".png"), overwrite = TRUE)
      }
      showNotification("Doctor Updated!", type="warning")
    }, error = function(e) { showNotification(paste("Error:", e$message), type="error") })
  })
  
  observeEvent(input$delete_doc, {
    req(input$doctor_table_rows_selected)
    id <- doctors_data()[input$doctor_table_rows_selected, "DoctorID"]
    tryCatch({
      dbExecute(con, sprintf("DELETE FROM Doctors WHERE DoctorID=%d", id))
      sig_file <- paste0("signatures/", id, ".png"); if(file.exists(sig_file)) file.remove(sig_file)
      showNotification("Doctor Deleted!", type="error")
    }, error = function(e) { showNotification("Cannot delete (Doctor has patients?)", type="error") })
  })
  
  # --- ADMIN & PDF ---
  output$admin_table <- renderDT({ datatable(users_data(), options=list(dom='t')) })
  observeEvent(input$add_admin_btn, { req(input$new_user, input$new_pass); tryCatch({ sql <- sprintf("INSERT INTO Users (Username, Password, SecurityQuestion, SecurityAnswer) VALUES ('%s', '%s', '%s', '%s')", input$new_user, input$new_pass, input$sec_q, input$sec_a); dbExecute(con, sql); showNotification("Admin Added", type="message") }, error = function(e) { showNotification("Error", type="error") }) })
  output$patient_table_pdf <- renderDT({ datatable(patients_data(), selection='single') })
  output$download_pdf <- downloadHandler(filename=function(){"report.pdf"}, content=function(file){ req(input$patient_table_pdf_rows_selected); render("patient_report.Rmd", output_format="pdf_document", output_file=file, params=list(patient=patients_data()[input$patient_table_pdf_rows_selected, ])) })
}

shinyApp(ui, server)