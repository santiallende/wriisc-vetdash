library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(vistime)
library(highcharter)
library(shinymanager)
library(reactable)
library(reactablefmtr)
library(vroom)
library(shinyjs)
library(hrbrthemes)
library(ggtext)

# JavaScript code to handle 'Enter' key behavior
#https://github.com/datastorm-open/shinymanager/issues/195
js <- "
pressbtn = function(){
    // Click the log in button
    document.getElementById('auth-go_auth').click();
};
window.onload = function() {
    // Password input field
    const field = document.getElementById('auth-user_pwd');

    // Add a function that preempts the 'Enter' key press
    field.addEventListener('keydown', function(e) {
        if (e.keyCode == 13) {
            // Prevent sending the key event
            e.preventDefault();
            // Delay activating the login button for 400 ms. Adjust time as needed
            setTimeout(pressbtn, 400);
        }
    });
}
"

# Define credentials
credentials <- data.frame(
  user = c("user"),
  password = c("pwd"),
  stringsAsFactors = FALSE
)

# UI
ui <- secure_app(
  dashboardPage(
    dashboardHeader(title = "WRIISC VetDash"),
    dashboardSidebar(
      sidebarMenu(
        selectizeInput("patient", "Select Patient",
          choices = unique(combined_selfreport_measures_dat$id),
          selected = unique(combined_selfreport_measures_dat$id[1]),
          width = "100%"
        ),
        menuItem("Patient Characteristics",
          tabName = "pt_characteristics",
          icon = icon("user")
        ),
        menuItem("Patient Health Symptoms",
          tabName = "pt_health_sx",
          icon = icon("heartbeat")
        ),
        menuItem("Patient Exposures",
          tabName = "pt_exposures",
          icon = icon("bolt")
        ),
        menuItem("Patient Self-Report Scores",
          tabName = "pt_self_report_scores",
          icon = icon("clipboard")
        )
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        ptCharacteristicsUI("firstMenuItem"),
        ptHealthSxUI("thirdMenuItem"),
        ptExposuresUI("secondMenuItem"),
        ptSelfReportUI("fourthMenuItem")
      )
    )
  ),
  timeout = 3600, 
  auto_disconnect = FALSE, 
  head_auth = tags$script(js) 
)

# Server
server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  selected_patient <- reactive({
    input$patient
  })

  ptCharacteristicsServer("firstMenuItem", selected_patient)
  ptExposuresServer("secondMenuItem", selected_patient)
  ptHealthSxServer("thirdMenuItem", selected_patient)
  ptSelfReportServer("fourthMenuItem", selected_patient)
}

# Run the app
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
