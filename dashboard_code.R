library(leaflet)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)   # For pivot_longer()

# Sample data: Amount of devices per country
device_data <- read_csv("merged_summary_with_lat_lon.csv", locale = locale(encoding = "UTF-8"))

# Sample data: GHG emissions reduction targets
emission_data <- data.frame(
  Year = c(2021, 2023, 2030, 2050),
  Policy = c("EU Climate Law", "IPCC Report", "IPCC Target", "Net Zero Goal"),
  Emissions = c(100, 95, 55, 0)  # Hypothetical % reduction
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$img(src = "images/logo1.jpg", style = "height: 110%; width: 120%; margin-left: -20px;")
  ),  
  
  dashboardSidebar(
    width = 250,
    
    # Second logo
    tags$div(
      style = "text-align:center; padding:10px;",
      tags$img(src = "images/logo2.jpg", width = "85%")
    ),
    
    sidebarMenu(
      id = "tabs",
      
      menuItem("About", tabName = "about", icon = icon("address-card")),
      menuItem("Dashboard", icon = icon("dashboard"),
               menuSubItem("Environmental", tabName = "dashboard1", icon = icon("leaf")),
               menuSubItem("Economic", tabName = "dashboard2", icon = icon("dollar-sign")),
               menuSubItem("Social", tabName = "dashboard3", icon = icon("users")),
               menuSubItem("Circularity Assessment", tabName = "dashboard4", icon = icon("chart-line"))
      ),
      menuItem("Settings", tabName = "settings", icon = icon("cogs")), 
      menuItem("Contact/Help", tabName = "contact", icon = icon("envelope"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #FF2953 !important;
          color: white !important;
          border-left-color: #77F0CC;
        }
        
        .skin-blue .sidebar-menu > li > ul > li.active > a {
          background-color: #2B2B65 !important;
          color: white !important;
        }

        .main-sidebar {
          background-color: #c2c2d6 !important;
        }
        
        .main-sidebar .sidebar .sidebar-menu a {
          color: #2B2B65 !important;
          font-weight: bold;
        }

        .skin-blue .sidebar-menu > li > ul {
          background-color: #CBCBD6 !important;
        }
        
        .skin-blue .sidebar-menu > li.active > a:hover,
        .skin-blue .sidebar-menu > li.menu-open > a:hover {
          background-color: #2B2B65 !important;
          color: white !important;
          cursor: default !important;
          border-left-color: #77F0CC !important;
        }

        .navbar {
          background-color: #2B2B65 !important;
        }

        .btn-custom {
          background-color: #CBCBD6 !important;
          color: black !important;
          border: none;
        }

        .btn:hover {
          background-color: #FF2953 !important;
        }

        .btn-active {
          pointer-events: none;
          background-color: #2B2B65 !important;
          color: white !important;
        }

        .skin-blue .sidebar-menu > li.treeview:hover > a {
          background-color: #FF2953 !important; 
          color:  white !important;
          border-left-color: #77F0CC;
        } 
    
        .btn-success {
          background-color: #28a745 !important;
          border-color: #28a745 !important;
        }

        .btn-danger {
          background-color: #dc3545 !important;
          border-color: #dc3545 !important;
        }

        .box {
          border-radius: 10px !important;
        }
        
        .shiny-notification {
          position: fixed;
          top: 60px;
          right: 20px;
          background-color: #2B2B65;
          color: white;
          padding: 10px;
          border-radius: 5px;
        }

        .box.box-solid.box-primary>.box-header {
          background:  #2B2B65;
          background-color: #2B2B65;
          border-radius: 10px 10px 0px 0px!important;
        }
        
        .box.box-solid.box-primary {
          border: 1px solid #2B2B65;
          border-radius: 10px !important;
        }

        .irs--shiny .irs-bar {
          border-top: 1px solid #2B2B65;
          border-bottom: 1px solid #2B2B65;
          background: #2B2B65;
        }

        .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
          color: #fff;
          background-color: #2B2B65;
        }

        input[type=checkbox], input[type=radio] {
          background-color: #2B2B65;
        }

        .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color:  #FF2953;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "about",
        h2("ABOUT Digital Health in Circular Economy (DiCE)"),
        p("EU funded project aiming to address the issue of increasing digital health waste."),
        p('Electronic waste (e-waste) from digital health devices is a complex and growing problem requiring a holistic solution. E-waste from healthcare products may pose biological or chemical contamination, leading to its incineration, with or without energy recovery. This means that all items are destroyed.

DiCE was created to bring key stakeholders together to address challenges associated with the growing use of digital healthcare products and increasing demand for raw materials to manufacture new electronic devices and other equipment.')
      ),
      
      tabItem(
        tabName = "dashboard1",
        h2("Environmental Overview"),
        actionButton("btn1", "Environmental", class = "btn btn-custom btn-active"),
        actionButton("btn2", "Economic", class = "btn btn-custom"),
        actionButton("btn3", "Social", class = "btn btn-custom"),
        actionButton("btn4", "Circularity Assessment", class = "btn btn-custom")
      ),
      
      tabItem(
        tabName = "dashboard2",
        h2("Economic Overview"),
        actionButton("btn1", "Environmental", class = "btn btn-custom"),
        actionButton("btn2", "Economic", class = "btn btn-custom btn-active"),
        actionButton("btn3", "Social", class = "btn btn-custom"),
        actionButton("btn4", "Circularity Assessment", class = "btn btn-custom")
      ),
      
      tabItem(
        tabName = "dashboard3",
        h2("Social Overview"),
        actionButton("btn1", "Environmental", class = "btn btn-custom"),
        actionButton("btn2", "Economic", class = "btn btn-custom"),
        actionButton("btn3", "Social", class = "btn btn-custom btn-active"),
        actionButton("btn4", "Circularity Assessment", class = "btn btn-custom"),
        br(),
        br(),
        
        fluidRow(
          titlePanel("Climate Policy Targets - IPCC & EU Climate Law"),
          
          sidebarLayout(
            sidebarPanel(
              selectInput("vizType", "Choose Visualization:",
                          choices = c("Timeline", "Progress Bar", "GHG Emissions Trend")),
              helpText("This dashboard visualizes key climate policy goals.")
            ),
            
            mainPanel(
              plotOutput("plot")
            )
          )
        )
        
      ),
      
      tabItem(
        tabName = "dashboard4",
        h2("Device Overview"),
        actionButton("btn1", "Environmental", class = "btn btn-custom"),
        actionButton("btn2", "Economic", class = "btn btn-custom"),
        actionButton("btn3", "Social", class = "btn btn-custom"),
        actionButton("btn4", "Circularity Assessment", class = "btn btn-custom btn-active"),
        br(),
        br(),
        
        fluidRow(
          box(title = "Select Country", width = 4, status = "primary", solidHeader = TRUE,
              selectInput("country_select", "Choose a country:", 
                          choices = c("Select a country", unique(device_data$`Country Name`)), 
                          selected = "Select a country"))
        ),
        
        fluidRow(
          box(title = "Device Distribution Map", width = 7, status = "primary", solidHeader = TRUE,
              leafletOutput("device_map", height = 800)),
          box(title = "Device Breakdown", width = 5, status = "primary", solidHeader = TRUE,
              plotlyOutput("device_pie", height = 800))
        )
      ),
      
      tabItem(
        tabName = "settings",
        h2("Settings"),
        box(title = "User Preferences", width = 6, status = "primary", solidHeader = TRUE,
            selectInput("theme", "Select Theme:", choices = c("Light", "Dark")),
            sliderInput("fontsize", "Font Size:", min = 10, max = 24, value = 14),
            radioButtons("sidebar_pos", "Sidebar Position:", choices = c("Left", "Right"), selected = "Left")
        ),
        
        fluidRow(
          column(6, actionButton("save_settings", "Save Settings", class = "btn btn-success")),
          column(6, actionButton("reset_settings", "Reset to Default", class = "btn btn-danger"))
        )
      ),
      
      tabItem(
        tabName = "contact",
        h2("Contact/Help"),
        p("This is the contact/help page. Please reach out to us for any queries.")
      )
    )
  )
)

# Define Server logic
server <- function(input, output
