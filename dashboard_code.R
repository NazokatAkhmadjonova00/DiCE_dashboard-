library(leaflet)  
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr) # For pivot_longer()
library(readr)
library(scales)

# Sample data: Amount of devices per country
device_data <- read_csv("merged_summary_with_lat_lon.csv", locale = locale(encoding = "UTF-8"))

# Read the CSV file
cost_data <- read_csv("cost_savings.csv")

# Reshape data for the line plot (long format)
cost_data_long <- cost_data %>%
  pivot_longer(cols = starts_with("Lifecycle"), 
               names_to = "Lifecycle", 
               values_to = "Cost")



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
      # Parent menu item: Dashboard with subItems
      
      menuItem("Dashboard", icon = icon("dashboard"),
               menuSubItem("Environmental", tabName = "dashboard1", icon = icon("leaf")),
               menuSubItem(" Economic", tabName = "dashboard2", icon = icon("dollar-sign")),
               menuSubItem("Social", tabName = "dashboard3", icon = icon("users")),
               menuSubItem("Circularity Assessment", tabName = "dashboard4", icon = icon("chart-line"))
      ),
      # Other menu items
      
      menuItem("Settings", tabName = "settings", icon = icon("cogs")), 
      menuItem("Contact/Help", tabName = "contact", icon = icon("envelope"))
      
      
      
      
      #div(
      #  style = "position: absolute; bottom: 10px; width: 85%; text-align: center;",
      #  actionButton("btn_contact", "Contact/Help", class = "btn btn-primary btn-block", style = "background-color: #2B2B65; color: white; border-color: #2B2B65;")
      #
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    
    
    tags$head(
      tags$style(HTML("
      
      
        /* Change the hover style for a specific menuItem */
        .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #FF2953 !important;
          color: white !important;
          border-left-color: #77F0CC;
        }
        
         /* Make sure the parent menu gets highlighted when a sub-menu is active */
    
   
 
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
          width: 100% !important;
        }
        
        /* Prevent hover effects on active menu items */
.skin-blue .sidebar-menu > li.active > a:hover,
.skin-blue .sidebar-menu > li.menu-open > a:hover
{
    background-color: #2B2B65 !important; /* Keep the active background */
    color: white !important; /* Keep the active text color */
    cursor: default !important; /* Prevent pointer change */
    border-left-color: #77F0CC !important;
}
        
       
        .skin-blue .sidebar-menu > li.active > a {
          background-color: #2B2B65 !important;
          color: white !important;
          border-left-color: #77F0CC;
        }
          
        
        .navbar {
          background-color: #2B2B65 !important;
        }

        .btn-custom {
          background-color: #CBCBD6 !important;
          color: black !important;
          border: none;
        }
        
        .btn:hover{
         background-color: #FF2953 !important;
        }

        .btn-active {
          pointer-events: none;
          background-color: #2B2B65 !important;
          color: white !important;
          outline: none !important;
        }



    /* Change the color of the parent menu item when hovering the sub-menu */
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
    .box.box-solid.box-primary>.box-header{
      background:  #2B2B65;
      background-color: #2B2B65;
      border-radius: 10px 10px 0px 0px!important;
    }
    
    .box.box-solid.box-primary{
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
    
    input[type=checkbox], input[type=radio]{
      background-color: #2B2B65;
    }
    
    .skin-blue .main-header .navbar .sidebar-toggle:hover {
    background-color:  #FF2953;
    }
    
        
  
      "))),
    
    
    tabItems(
      # Dashboard Page 1 (Environmental)
      tabItem(
        tabName = "about",
        h2("ABOUT Digital Health in Circular Economy (DiCE)"),
        p("EU funded project aiming to address the issue of increasing digital health waste."),
        p('Electronic waste (e-waste) from digital health devices is a complex and growing problem requiring a holistic solution. E-waste from healthcare products may pose biological or chemical contamination, leading to its incineration, with or without energy recovery. This means that all items are destroyed.

DiCE was created to bring key stakeholders together to address challenges associated with the growing use of digital healthcare products and increasing demand for raw materials to manufacture new electronic devices and other equipment.')
        
      ),
      # Dashboard Page 1 (Environmental)
      tabItem(
        tabName = "dashboard1",
        h2("Environmental Overview"),
        actionButton("btn1", "Environmental", class = "btn btn-custom btn-active"),
        actionButton("btn2", "Economic", class = "btn btn-custom"),
        actionButton("btn3", "Social", class = "btn btn-custom"),
        actionButton("btn4", "Circularity Assessment", class = "btn btn-custom")
      ),
      
      # Dashboard Page 2 (Economic)
      tabItem(
        tabName = "dashboard2",
        h2("Economic Overview"),
        actionButton("btn1", "Environmental", class = "btn btn-custom"),
        actionButton("btn2", "Economic", class = "btn btn-custom btn-active"),
        actionButton("btn3", "Social", class = "btn btn-custom"),
        actionButton("btn4", "Circularity Assessment", class = "btn btn-custom"),
        br(),
        br(),
        
        fluidRow(
          box(title = "Cost Comparison Across Lifecycles", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("line_plot", height = 400)),
          box(title = "Total Cost Savings per Recovery Rate", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("bar_plot", height = 400))
        )
        
        
        
        
      ),
      
      # Dashboard Page 3 (Social)
      tabItem(
        tabName = "dashboard3",
        h2("Social Overview"),
        actionButton("btn1", "Environmental", class = "btn btn-custom"),
        actionButton("btn2", "Economic", class = "btn btn-custom"),
        actionButton("btn3", "Social", class = "btn btn-custom btn-active"),
        actionButton("btn4", "Circularity Assessment", class = "btn btn-custom"),
        br(),
        br(),
        
        p("European Climate Law sets the intermediate target of reducing net greenhouse gas emissions by at least 55% by 2030, compared to 1990 levels.
          Thus the plot that will run trough the years 1990 to 2050 "),
        
        #Target variable visualisation 
        fluidRow(
          titlePanel("Climate Policy Targets - IPCC & EU Climate Law")
        )
        
        
      ),
      
      # Analytics Page
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
              leafletOutput("device_map", height = 700)),
          box(title = "Device Breakdown", width = 5, status = "primary", solidHeader = TRUE,
              plotlyOutput("device_pie", height = 700))
          
          
        )
      ),
      
      # Settings Page
      tabItem(
        tabName = "settings",
        h2("Settings"),
        # User Preferences
        box(title = "User Preferences", width = 6, status = "primary", solidHeader = TRUE,
            #selectInput("theme", "Select Theme:", choices = c("Light", "Dark")),
            sliderInput("fontsize", "Font Size:", min = 10, max = 24, value = 14)#,
            # radioButtons("sidebar_pos", "Sidebar Position:", choices = c("Left", "Right"), selected = "Left")
        ),
        fluidRow(
          column(6)),
        
        br(),
        
        # Notifications
        # box(title = "Notifications", width = 6, status = "primary", solidHeader = TRUE,
        #    checkboxInput("email_notif", "Receive Email Notifications", TRUE),
        #   checkboxInput("push_notif", "Receive Push Notifications", FALSE)
        #),
        
        # Account Settings
        #box(title = "Account Settings", width = 6, status = "warning", solidHeader = TRUE,
        #   textInput("username", "Change Username:", ""),
        #   passwordInput("password", "Change Password:")
        #),
        
        # Action Buttons
        fluidRow(
          column(2, actionButton("save_settings", "Save Settings", class = "btn btn-success", style="margin-left:10px;")),
          
          
          column(10, actionButton("reset_settings", "Reset to Default", class = "btn btn-danger"))
          
        )
      ),
      
      # Add the Contact Tab
      tabItem(
        tabName = "contact",
        h2("Contact/Help"),
        p("This is the contact/help page. Please reach out to us for any queries.")
      )
    ) #correct
  )
)






# Define Server logic
server <- function(input, output, session) {
  
  
  
  # Observe btn1 (Environmental) click: Switch to Dashboard 1 and make btn1 active
  observeEvent(input$btn1, {
    
    updateTabItems(session, "tabs", "dashboard1")
  })
  
  # Observe btn2 (Economic) click: Switch to Dashboard 2 and make btn2 active
  observeEvent(input$btn2, {
    
    updateTabItems(session, "tabs", "dashboard2")
  })
  
  # Observe btn3 (Social) click: Switch to Dashboard 3 and make btn3 active
  observeEvent(input$btn3, {
    
    updateTabItems(session, "tabs", "dashboard3")
  })
  
  observeEvent(input$btn4, {
    
    updateTabItems(session, "tabs", "dashboard4")
  })
  
  # Observe btn3 (Social) click: Switch to Dashboard 3 and make btn3 active
  observeEvent(input$btn_contact, {
    
    updateTabItems(session, "tabs", "contact")
  })
  
  
  
  
  # Line plot: Compare costs across lifecycles by recovery rate
  output$line_plot <- renderPlotly({
    ggplot(cost_data_long, aes(x = Lifecycle, y = Cost, group = `Recovery rate`, color = as.factor(`Recovery rate`))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = "Cost Comparison Across Lifecycles",
           x = "Lifecycle",
           y = "Cost ($)",
           color = "Recovery Rate") +
      scale_color_manual(values = c("92%" = "#2B2B65", "80%" = "#FF2953", "60%" = "#006400", "40%" = "#77F0CC")) +  # Custom colors
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })  
  # Bar plot: Total cost savings per recovery rate
  output$bar_plot <- renderPlotly({
    ggplot(cost_data, aes(x = as.factor(`Recovery rate`), y = `Total costs`, fill = as.factor(`Recovery rate`))) +
      geom_bar(stat = "identity", show.legend = FALSE, fill = "#2B2B65") +  # Set the fill color to dark blue
      labs(title = "Total Cost Savings per Recovery Rate",
           x = "Recovery Rate",
           y = "Total Cost Savings ($)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  
  # Default values for settings
  default_settings <- reactiveValues(
    fontsize = 14
  )
  
  # Observe Save Button Click
  observeEvent(input$save_settings, {
    showNotification("Settings Saved!", type = "message")
    
    # Store user preferences
    default_settings$fontsize <- input$fontsize
    
    # Apply Font Size
    font_size_css <- paste0("body { font-size: ", input$fontsize, "px !important; }")
    shinyjs::runjs(paste0("var style = document.createElement('style'); style.innerHTML = '", font_size_css, "'; document.head.appendChild(style);"))
    
    # Apply Font Size to Sidebar Sub-items
    sidebar_font_size_css <- paste0(".sidebar-menu .treeview-menu > li > a { font-size: ", input$fontsize, "px !important; }")
    shinyjs::runjs(paste0("var style = document.createElement('style'); style.innerHTML = '", sidebar_font_size_css, "'; document.head.appendChild(style);"))
  })
  
  # Observe Reset Button Click
  observeEvent(input$reset_settings, {
    showNotification("Settings Reset!", type = "warning")
    
    # Reset all inputs to default values
    updateSliderInput(session, "fontsize", value = 14)
    
    # Reset font size to default
    shinyjs::runjs("var style = document.createElement('style'); style.innerHTML = 'body { font-size: 14px !important; }'; document.head.appendChild(style);")
    
    # Reset font size for sidebar sub-items
    shinyjs::runjs("var style = document.createElement('style'); style.innerHTML = '.sidebar-menu .treeview-menu > li > a { font-size: 14px !important; }'; document.head.appendChild(style);")
  })
  
  
  #render Plot
  output$plot <- renderPlot({
    
    if (input$vizType == "Timeline") {
      # Timeline Visualization
      ggplot(emission_data, aes(x = Year, y = 1, label = Policy)) +
        geom_point(size = 4, color = "blue") +
        geom_text(vjust = -1, hjust = 0.5, size = 5) +
        geom_segment(aes(xend = Year, yend = 1), color = "gray", size = 1) +
        labs(title = "Climate Policy Timeline",
             x = "Year", y = "Milestones") +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank())
      
    } else if (input$vizType == "Progress Bar") {
      # Progress Bar Visualization
      ggplot(emission_data, aes(x = factor(Year), y = Emissions, fill = Policy)) +
        geom_col(width = 0.5) +
        coord_flip() +
        scale_y_continuous(labels = percent_format(scale = 1)) +
        labs(title = "Emissions Reduction Progress",
             x = "Year", y = "Emissions Reduction (%)") +
        theme_minimal()
      
    } else {
      # Line Chart - GHG Emissions Reduction Over Time
      ggplot(emission_data, aes(x = Year, y = Emissions, group = 1)) +
        geom_line(color = "red", size = 1.5) +
        geom_point(size = 4, color = "red") +
        labs(title = "GHG Emissions Reduction Trend",
             x = "Year", y = "Emissions Reduction (%)") +
        theme_minimal()
    }
  })
  
  
  library(leaflet)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  
  
  
  # Sample data: Amount of devices per country
  device_data <- read_csv("merged_summary_with_lat_lon.csv", locale = locale(encoding = "UTF-8"))
  # Reactive: Track selected country (from map OR dropdown)
  selected_country <- reactiveVal(NULL)
  
  
  output$device_map <- renderLeaflet({
    leaflet(device_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, radius = ~sqrt(Total_Devices) /10,
        color =  "#FF2953", fillOpacity = 0.6,
        popup = ~paste0("<b>", `Country Name`, "</b><br>Total Devices: ", Total_Devices),
        layerId = ~`Country Name`
      )
  })
  
  # Update selection when map marker is clicked
  observeEvent(input$device_map_marker_click, {
    selected_country(input$device_map_marker_click$id)
    updateSelectInput(session, "country_select", selected = input$device_map_marker_click$id)
  })
  
  # Update selection when dropdown is used
  observeEvent(input$country_select, {
    if (input$country_select != "Select a country") {
      selected_country(input$country_select)
    }
  })
  
  # Render Pie Chart
  output$device_pie <- renderPlotly({
    req(selected_country())  # Only update if a country is selected
    
    # Filter the data for the selected country
    country_data <- device_data %>% filter(`Country Name` == selected_country())
    
    # Prepare data for the pie chart
    pie_data <- data.frame(
      Category = c("Multi-use Devices", "Reprocessed Devices", "Single-use Devices"),
      Amount = c(country_data$`Multi use`, country_data$Reprocessed, country_data$Singleuse)
    )
    
    # Render the pie chart
    plot_ly(pie_data, labels = ~Category, values = ~Amount, type = "pie",
            marker = list(colors = c("#2B2B65", "#FF2953", "#77F0CC"))) %>%
      layout(title = paste("Device Breakdown for", country_data$`Country Name`))
    
    
    bar_data <- data.frame(
      Category = c("Multi-use Devices", "Reprocessed Devices", "Single-use Devices"),  # Custom names for categories
      Amount = c(country_data$`Multi use`, country_data$Reprocessed, country_data$Singleuse)  # Match to your column names
    )
    
    # Create the bar chart using plotly
    plot_ly(bar_data, x = ~Category, y = ~Amount, type = "bar",
            marker = list(color = c("#2B2B65", "#FF2953", "#77F0CC"))) %>%
      layout(title = paste("Device Breakdown for", country_data$`Country Name`),
             xaxis = list(title = "Device Type"),
             yaxis = list(title = "Amount"))
  })
  
  
  
}

# Run the  app
shinyApp(ui = ui, server = server)
