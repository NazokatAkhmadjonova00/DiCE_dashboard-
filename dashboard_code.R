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
library(viridis)
library(shinyWidgets)

# Load dataset
climate_change_data <- read.csv("climate_change.csv", sep=",", header=TRUE)

# Sample data: Amount of devices per country
#device_data <- read_csv("merged_summary_with_lat_lon.csv", locale = locale(encoding = "UTF-8"))

# Read the CSV file
cost_data <- read_csv("cost_savings.csv")

# Reshape data for the line plot (long format)
cost_data_long <- cost_data %>%
  pivot_longer(cols = starts_with("Lifecycle"), 
               names_to = "Lifecycle", 
               values_to = "Cost")
# Read in the dataset
zion_data <- read.csv("zion_volume_rep.csv")

# Prepare data (assuming that "Year" is a column with the years and "Volume" is the volume per country for each year)
years <- as.character(2018:2032)


# Sample data: GHG emissions reduction targets
emission_data <- data.frame(
  Year = c(2021, 2023, 2030, 2050),
  Policy = c("EU Climate Law", "IPCC Report", "IPCC Target", "Net Zero Goal"),
  Emissions = c(100, 95, 55, 0)  # Hypothetical % reduction
)
volume_range <- range(zion_data[, 2:10], na.rm = TRUE)
color_palette <- colorNumeric(
  palette = "viridis",      # Choose your color palette (can use other like 'inferno', 'magma', etc.)
  domain = volume_range     # Fixed range across all years
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
               menuSubItem("Economic", tabName = "dashboard2", icon = icon("dollar-sign")),
               menuSubItem("Circularity Assessment", tabName = "dashboard3", icon = icon("users"))
               #,
              # menuSubItem("Circularity Assessment", tabName = "dashboard4", icon = icon("chart-line"))
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
        
        #infoButton{
          border-color: white;
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
    .dropdown-menu>.active>a, .dropdown-menu>.active>a:focus, .dropdown-menu>.active>a:hover {
    color: #fff;
    text-decoration: none;
    background-color: #2B2B65;
    outline: 0;
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
        fluidRow(
                 column(6,
          
        h2("Environmental Overview"),
        actionButton("btn1", "Environmental", class = "btn btn-custom btn-active"),
        actionButton("btn2", "Economic", class = "btn btn-custom"),
        actionButton("btn3", "Circularity Assessment", class = "btn btn-custom"),
        #actionButton("btn4", "Circularity Assessment", class = "btn btn-custom"),
        ),
        column(2),
        br(),
        
        # Device type selection dropdown
        column(6,
               box(
               pickerInput(
                 inputId = "deviceType",
                 label = "Select Device Type",
                 choices = c("Endocutter", "DDL", "ECG leadset", "Smart pillbox"),
                 selected = "Endocutter",
                 multiple = FALSE, 
                 choicesOpt = list(
                   disabled = c(FALSE, TRUE, TRUE, TRUE)  # Disable all except "Endocutter"
                 
               ),
               textOutput("selectedDevice")  # Display selected option
        ))
        )),
      
          
        fluidRow(
          box(title = "Climate Change Impact by Process (Endocutter)", width = 11, solidHeader = TRUE, status = "primary",
              plotlyOutput("barPlot"))
          )
        
      ),
      
      # Dashboard Page 2 (Economic)
      tabItem(
        tabName = "dashboard2",
        fluidRow(
          column(6,
        h2("Economic Overview"),
        actionButton("btn1", "Environmental", class = "btn btn-custom"),
        actionButton("btn2", "Economic", class = "btn btn-custom btn-active"),
        actionButton("btn3", "Circularity Assessment", class = "btn btn-custom"),
        #actionButton("btn4", "Circularity Assessment", class = "btn btn-custom"),
          ),
        column(2),
        br(),
        
        # Device type selection dropdown
        column(6,
               box(
                 pickerInput(
                   inputId = "deviceType",
                   label = "Select Device Type",
                   choices = c("Endocutter", "DDL", "ECG leadset", "Smart pillbox"),
                   selected = "DDL",
                   multiple = FALSE, 
                   choicesOpt = list(
                     disabled = c(TRUE, FALSE, TRUE, TRUE)  # Disable all except "Endocutter"
                     
                   ),
                   textOutput("selectedDevice")  # Display selected option
                 ))
        )),
        br(),
        
        fluidRow(
          box(title = "Supply Chain Costs across DDL lifecycles", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("line_plot", height = 400)),
          box(title = "Total Cost Savings per Recovery Rate (DDL)", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("bar_plot", height = 400))
        )
      ),
      
      # Dashboard Page 3 (Social)
      tabItem(
        tabName = "dashboard3",
        h2("Circularity Assessment"),
        actionButton("btn1", "Environmental", class = "btn btn-custom"),
        actionButton("btn2", "Economic", class = "btn btn-custom"),
        actionButton("btn3", "Circularity Assessment", class = "btn btn-custom btn-active"),
        #actionButton("btn4", "Circularity Assessment", class = "btn btn-custom"),
        br(),
        br(),
        
        # Second logo
        
        
      #  box(title = "Cost Comparison Across Lifecycles", width = 10, status = "primary", solidHeader = TRUE,
      #      tags$div(
      #        style = "text-align:center; padding:10px;",
      #        tags$img(src = "images/authorized_map.png", width = "85%")
      #      )),
        
        fluidRow(
          
          box( title = tagList(
            "Volume of Reprocessed Digital Health Devices in Europe", 
            actionButton("infoButton", label = NULL, icon = icon("info-circle"), class = "btn btn-info", style = "margin-left: 10px;")
          ), 
          status = "primary", solidHeader = TRUE,
            leafletOutput("map", height = 400), width = 12)
        
          ),
        
        fluidRow(
          box(width = 12, align = "center",  # Centered horizontal slider
              sliderInput("yearSlider", "Select Year", 
                          min = 2018, max = 2032, value = 2024, step = 1, 
                          animate = TRUE, width = "100%", sep = ""))
        )
        
      ),
      
      # Analytics Page
     # tabItem(
      #  tabName = "dashboard4",
       # fluidRow(
       #   column(6, h2("Device Overview"),
       #          actionButton("btn1", "Environmental", class = "btn btn-custom"),
       #          actionButton("btn2", "Economic", class = "btn btn-custom"),
       #          actionButton("btn3", "Circularity Assessment", class = "btn btn-custom")
                 #,
                 #actionButton("btn4", "Circularity Assessment", class = "btn btn-custom btn-active")
       #   ),
        #  column(2),
        #  column(4, 
         #        box(title = "Select Country", width = NULL, status = "primary", solidHeader = TRUE,
        #             selectInput("country_select", label = NULL, 
          #                       choices = c("Select a country", unique(device_data$`Country Name`)), 
         #                        selected = "Select a country"))
         # )),
        
        
    #  fluidRow(
    #      box(title = "Device Distribution Map", width = 7, status = "primary", solidHeader = TRUE,
    #          leafletOutput("device_map", height = 400)),
    #      box(title = "Device Breakdown", width = 5, status = "primary", solidHeader = TRUE,
    #          plotlyOutput("device_pie", height = 400))
          
          
    #    )
    #  ),
      
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
  
#  observeEvent(input$btn4, {
    
 #   updateTabItems(session, "tabs", "dashboard4")
  #})
  
  # Observe btn3 (Social) click: Switch to Dashboard 3 and make btn3 active
  observeEvent(input$btn_contact, {
    
    updateTabItems(session, "tabs", "contact")
  })
  
  
  output$barPlot <- renderPlotly({
    # Filter out total impact for stacking
    stacked_data <- climate_change_data %>% filter(Category != "Total impact")
    total_impact <- climate_change_data %>% filter(Category == "Total impact")
    
    # Create stacked bar chart with total impact overlay
    p <- ggplot() +
      geom_bar(data = stacked_data, aes(x = Process, y = Kg_CO2_eq, fill = Category), 
               stat = "identity") +
      geom_point(data = total_impact, aes(x = Process, y = Kg_CO2_eq), 
                 shape = 18, size = 3, color = "black") +  # Diamond shape for total impact
     
      
      geom_text(vjust = -1, hjust = 0.5, size = 5) +
      theme_minimal() +
      ylab("Kg CO₂-eU") +
      #ggtitle("Climate Change Impact by Process") +
      scale_fill_brewer(palette = "Set2") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 14, face = "bold"),
            legend.position = "bottom",  # Move legend to the bottom
            legend.title = element_blank(),  # Remove legend title
            legend.box = "horizontal") 
    
    ggplotly(p)  # Convert ggplot to interactive plotly
  })
  
  
  # Line plot: Compare costs across lifecycles by recovery rate
  output$line_plot <- renderPlotly({
    ggplot(cost_data_long, aes(x = Lifecycle, y = Cost, group = `Recovery rate`, color = as.factor(`Recovery rate`))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
           x = "Lifecycle",
           y = "Cost (1000 euros)",
           color = "Recovery Rate") +
      scale_color_manual(values = c("92%" = "#2B2B65", "80%" = "#FF2953", "60%" = "#006400", "40%" = "#77F0CC")) +  # Custom colors
      scale_y_continuous(labels = label_number(scale = 0.001)) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })  
  # Bar plot: Total cost savings per recovery rate
  output$bar_plot <- renderPlotly({
    ggplot(cost_data, aes(x = as.factor(`Recovery rate`), y = `Total costs`, fill = as.factor(`Recovery rate`))) +
      geom_bar(stat = "identity", show.legend = FALSE, fill = "#2B2B65") + 
      geom_text(aes(label = label_comma()(round(`Total costs`/1000, 1))), 
                vjust = -3, color = "#FF2953", size = 4) +# Set the fill color to dark blue
      labs(
           x = "Recovery Rate",
           y = "Total Cost Savings (1000 euros)") +
      scale_y_continuous(labels = label_comma(scale = 0.001))  +
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
  
  # Ensure selectedData is defined before using it
  selectedData <- reactive({
    year_col <- paste0("X", input$yearSlider)  # Dynamically get the column for the selected year
    zion_data %>%
      select(Countries, Longitude, Latitude, all_of(year_col)) %>%
      rename(Volume = all_of(year_col))  # Rename for easier mapping
  })
  
  # Define fixed range for color scale
  volume_range <- c(300, 75000)
  
  # Reactive color palette with a fixed domain
  colorPalette <- reactive({
    colorNumeric(palette = "plasma", domain = volume_range)  # Fixed range
  })
  
  output$map <- renderLeaflet({
    data <- selectedData()  # Get initial data
    pal <- colorPalette()   # Use fixed palette
    
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = 10, lat = 50, zoom = 4) %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        #radius = ~sqrt(Volume) / 5,
        radius = 12,
        fillColor = ~pal(Volume),
        color = "black",  # Black border for visibility
        fillOpacity = 0.8,
        weight = 1,
        popup = ~paste0("<b>", Countries, "</b><br/>",
                        "Year: ", input$yearSlider, "<br/>",
                        "Volume: ", Volume)
      ) %>%
      addLegend(
        "bottomright",
        pal = pal, 
        values = volume_range,  # Fixed range
        title = "Device Volume",
        opacity = 1,
        labFormat = labelFormat(suffix = " units") 
      )
  })
  
  observe({
    data <- selectedData()
    pal <- colorPalette()
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        #radius = ~sqrt(Volume) / 5,
        radius = 12,
        fillColor = ~pal(Volume),
        color = "black",  # Fixed border color
        fillOpacity = 0.8,
        weight = 1,
        popup = ~paste0("<b>", Countries, "</b><br/>",
                        "Year: ", input$yearSlider, "<br/>",
                        "Volume: ", Volume)
      ) %>%
      clearControls() %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = c(300, 75000),  # Use fixed min-max
        title = "Device Volume",
        opacity = 1
      )
  })
  # When the info button is clicked, show the modal with description
  observeEvent(input$infoButton, {
    showModal(modalDialog(
      title = "Europe Reprocessed Digital Health Devices Market by Countries, Volume (Units), 2018 - 2032",
      p("This section visualizes the market for reprocessed digital health devices over the years."),
      p("The map below shows the distribution of reprocessed digital health devices, with markers representing the volume of devices in each country."),
      p("The data from 2018 to 2023 represents historical years, with actual data for those years."),
      p("2023 is the base year, where the data is considered most accurate and serves as the foundation for future projections."),
      p("The years 2024 to 2032 are projections, based on estimates of market trends, future developments, and expected growth in the digital health devices market."),
      p("You can use the slider below to change the year and see how the market has evolved historically and how it is projected to change in the future."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
 # library(leaflet)
#  library(ggplot2)
 # library(plotly)
 # library(dplyr)
  
  
  
  # Sample data: Amount of devices per 
 # device_data <- read_csv("merged_summary_with_lat_lon.csv", locale = locale(encoding = "UTF-8"))
  # Reactive: Track selected country (from map OR dropdown)
#  selected_country <- reactiveVal(NULL)
  
  
 # output$device_map <- renderLeaflet({
  #  leaflet(device_data) %>%
   #   addTiles() %>%
   #   addCircleMarkers(
  #      lng = ~Longitude, lat = ~Latitude, radius = ~sqrt(Total_Devices) /40,
  #      color =  "#FF2953", fillOpacity = 0.6,
  #      popup = ~paste0("<b>", `Country Name`, "</b><br>Total Devices: ", Total_Devices),
  #      layerId = ~`Country Name`
  #    )
#  })
  
  # Update selection when map marker is clicked
 # observeEvent(input$device_map_marker_click, {
#    selected_country(input$device_map_marker_click$id)
 #   updateSelectInput(session, "country_select", selected = input$device_map_marker_click$id)
 # })
  
  # Update selection when dropdown is used
 # observeEvent(input$country_select, {
#    if (input$country_select != "Select a country") {
#      selected_country(input$country_select)
#    }
 # })
  

    
    # Filter the data for the selected country
 #   country_data <- device_data %>% filter(`Country Name` == selected_country())
    
    # Prepare data for the pie chart
#    pie_data <- data.frame(
#      Category = c("Multi-use Devices", "Reprocessed Devices", "Single-use Devices"),
#      Amount = c(country_data$`Multi use`, country_data$Reprocessed, country_data$Singleuse)
#    )
   #
    
#    bar_data <- data.frame(
#      Category = c("Multi-use Devices", "Reprocessed Devices", "Single-use Devices"),  # Custom names for categories
#      Amount = c(country_data$`Multi use`, country_data$Reprocessed, country_data$Singleuse)  # Match to your column names
#    )
    
    # Create the bar chart using plotly
#    plot_ly(bar_data, x = ~Category, y = ~Amount, type = "bar",
#            marker = list(color = c("#2B2B65", "#FF2953", "#77F0CC"))) %>%
#      layout(title = paste("Device Breakdown for", country_data$`Country Name`),
#             xaxis = list(title = "Device Type"),
#             yaxis = list(title = "Amount"))
#  })
  
  
  
}

# Run the  app
shinyApp(ui = ui, server = server)
