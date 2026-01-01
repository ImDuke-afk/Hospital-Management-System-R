# Hospital-Management-System-R
"A complete hospital admin dashboard built with R Shiny and MySQL."
# Hospital Management System (R Shiny + MySQL)

A complete Patient & Doctor Management System built with R Shiny.

## Features
* **Secure Login:** Admin authentication with password recovery.
* **Dashboard:** Live stats for Patients, High Risk cases, and Doctors.
* **Patient Management:** Add, Update, Delete patient records.
* **Doctor Management:** Manage staff, assign rooms, and upload signatures.
* **PDF Reports:** Generate official signed medical reports.

## How to Run
1. Install R and RStudio.
2. Install required packages in RStudio:
   ```r
   install.packages(c("shiny", "shinydashboard", "DBI", "RMySQL", "dplyr", "DT", "rmarkdown", "shinyjs"))
   Setup the Database:

Install MySQL (e.g., via XAMPP).

Create a database named HospitalDB.

Create the tables Patients, Doctors, and Users (you can include your SQL structure here).

Open app.R in RStudio and click Run App.
