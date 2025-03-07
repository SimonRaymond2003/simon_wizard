# File: ui.R
library(shiny)
library(leaflet)
library(shinyjs)
library(shinyWidgets)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style(HTML("
      :root {
        --primary-color: #2F6B52;
        --primary-light: #3d8b6b;
        --primary-dark: #245240;
        --secondary-color: #1E88E5;
        --secondary-light: #64B5F6;
        --secondary-dark: #1565C0;
        --background: #f8fafc;
        --card-bg: #ffffff;
        --text-primary: #2d3748;
        --text-secondary: #4a5568;
        --border-color: #e2e8f0;
      }

      body {
        font-family: 'Inter', sans-serif;
        background-color: var(--background);
        color: var(--text-primary);
        line-height: 1.5;
      }

      .well {
        background-color: var(--card-bg);
        border: 1px solid var(--border-color);
        border-radius: 12px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        padding: 20px;
      }

      .nav-tabs {
        border-bottom: 2px solid var(--border-color);
        margin-bottom: 20px;
      }

      .nav-tabs > li > a {
        color: var(--text-secondary);
        border: none;
        border-bottom: 2px solid transparent;
        margin-right: 20px;
        padding: 10px 0;
        font-weight: 500;
        transition: all 0.3s ease;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li > a:hover {
        color: var(--primary-color);
        background: none;
        border: none;
        border-bottom: 2px solid var(--primary-color);
      }

      .form-control {
        border-radius: 8px;
        border: 1px solid var(--border-color);
        padding: 8px 12px;
        transition: all 0.3s ease;
      }

      .form-control:focus {
        border-color: var(--primary-color);
        box-shadow: 0 0 0 2px rgba(47, 107, 82, 0.1);
      }

      h4 {
        color: var(--text-primary);
        font-weight: 600;
        margin-bottom: 1.5rem;
      }
      
      .loading {
        display: none;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1000;
        background: rgba(255, 255, 255, 0.95);
        padding: 24px 32px;
        border-radius: 16px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.15);
        text-align: center;
        backdrop-filter: blur(5px);
      }
      
      .loading-wheel {
        width: 40px;
        height: 40px;
        margin: 1rem auto;
        border: 3px solid #f3f3f3;
        border-top: 3px solid #2F6B52;
        border-radius: 50%;
        animation: spin 1s linear infinite;
      }
      
      .loading-text {
        margin-top: 10px;
        color: #2F6B52;
        font-weight: 500;
      }
      
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }

      .instruction-box {
        background: #f8fafc;
        border-radius: 8px;
        padding: 16px;
        margin: 16px 0;
        border-left: 4px solid var(--primary-color);
      }

      .instruction-box h5 {
        color: var(--primary-color);
        margin-bottom: 12px;
        font-weight: 600;
      }

      .map-instruction {
        animation: fadeInDown 0.5s ease;
      }

      .section-card {
        background: var(--card-bg);
        border-radius: 12px;
        border: 1px solid var(--border-color);
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        transition: all 0.3s ease;
      }

      .section-header {
        color: var(--primary-color);
        font-weight: 600;
        padding-bottom: 12px;
        margin-bottom: 20px;
        border-bottom: 2px solid var(--border-color);
      }

      .btn {
        border-radius: 8px;
        padding: 8px 16px;
        font-weight: 500;
        transition: all 0.3s ease;
      }

      .btn-primary {
        background-color: var(--primary-color) !important;
        border-color: var(--primary-color) !important;
        color: white !important;
      }

      .btn-blue {
        background-color: var(--secondary-color) !important;
        border-color: var(--secondary-color) !important;
        color: white !important;
      }

      .btn-secondary {
        background-color: #f7fafc !important;
        border-color: var(--border-color) !important;
        color: var(--text-secondary) !important;
      }
    "))
  ),
  
  titlePanel("Nova Scotia Housing Wizard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Sale Finder inputs
      conditionalPanel(
        condition = "input.tabs == 'Sale Finder'",
        tags$div(class = "section-card",
                 tags$h4("Sale Finder Options", class = "section-header"),
                 sliderInput("sale_year_range", 
                             "Sale Year Range:", 
                             min = sales_res_char_assess$year_range[1],
                             max = sales_res_char_assess$year_range[2],
                             value = c(2020, 2024),
                             step = 1,
                             sep = ""),
                 
                 selectInput("sale_filter_type", 
                             "Select Filter Type:", 
                             choices = c("Area", "Street", "Postal Code", "City", "3-digit Postal Code")),
                 
                 conditionalPanel(
                   condition = "input.sale_filter_type == 'Area'",
                   sliderInput("sale_radius", 
                               "Select radius (meters):", 
                               min = 100,
                               max = 1000,
                               value = 500,
                               step = 100)
                 ),
                 
                 conditionalPanel(
                   condition = "input.sale_filter_type == 'Street'",
                   textInput("sale_street_name", "Enter Street Name:")
                 ),
                 
                 conditionalPanel(
                   condition = "input.sale_filter_type == 'Postal Code'",
                   textInput("sale_postal_code", "Enter Postal Code:")
                 ),
                 
                 conditionalPanel(
                   condition = "input.sale_filter_type == 'City'",
                   textInput("sale_city_name", "Enter City Name:")
                 ),
                 
                 conditionalPanel(
                   condition = "input.sale_filter_type == '3-digit Postal Code'",
                   textInput("sale_postal_code_3digit", "Enter 3-digit Postal Code:")
                 ),
                 
                 actionButton("clear_sale", "Clear", 
                              class = "btn-secondary",
                              style = "width: 100%;")
        )
      ),
      
      # Property Finder inputs
      conditionalPanel(
        condition = "input.tabs == 'Property Finder'",
        tags$div(class = "section-card",
                 tags$h4("Property Finder Options", class = "section-header"),
                 
                 tags$style(HTML("
                   .buyer-profiles {
                     max-width: 280px;
                     margin: 5px auto 15px;
                   }
                   .buyer-profiles .control-label {
                     display: none;
                   }
                   .buyer-profiles input[type='radio'] {
                     position: absolute;
                     opacity: 0;
                   }
                   .buyer-profiles label {
                     font-size: 0.95em;
                     padding: 6px 12px;
                     font-weight: 500;
                     border-radius: 6px;
                     width: 100%;
                     display: block;
                     margin: 10px 0;
                     transition: all 0.3s ease;
                     cursor: pointer;
                     background: var(--primary-color);
                     color: white;
                     text-align: center;
                   }
                   .buyer-profiles label:hover {
                     background: var(--primary-light);
                     transform: translateY(-1px);
                     box-shadow: 0 2px 8px rgba(47, 107, 82, 0.2);
                   }
                   .buyer-profiles input[type='radio']:checked + label {
                     background: var(--primary-light);
                   }
                   .buyer-profiles input[type='radio']:checked ~ div {
                     display: none;
                   }
                 ")),
                 div(class = "buyer-profiles",
                     radioButtons("buyer_profile", "",
                                  choices = c(
                                    "Young Professional/Couple" = "young_prof",
                                    "New Family" = "new_family",
                                    "Family with Children" = "family",
                                    "Retiree" = "retiree"
                                  ),
                                  selected = character(0))
                 ),
                 
                 conditionalPanel(
                   condition = "input.buyer_profile",
                   sliderInput("nearby_matches", "Number of nearby matches:", 
                               min = 10, max = 500, 
                               value = 100, step = 10),
                   
                   sliderInput("find_price_range", "Price Range:", 
                               min = sales_res_char_assess$price_range[1], 
                               max = sales_res_char_assess$price_range[2],
                               value = c(sales_res_char_assess$price_range[1], sales_res_char_assess$price_range[2]),
                               step = 10000, pre = "$"),
                   
                   sliderInput("find_sqft_range", "Square Footage Range:", 
                               min = sales_res_char_assess$sqft_range[1],
                               max = sales_res_char_assess$sqft_range[2],
                               value = c(sales_res_char_assess$sqft_range[1], sales_res_char_assess$sqft_range[2]),
                               step = 100, post = " sqft"),
                   
                   sliderInput("find_bedrooms_range", "Number of Bedrooms (6+ means 6 or more):", 
                               min = sales_res_char_assess$bedrooms_range[1],
                               max = 6,
                               value = c(sales_res_char_assess$bedrooms_range[1], 6),
                               step = 1,
                               post = "+"),
                   
                   sliderInput("find_bathrooms_range", "Number of Bathrooms (6+ means 6 or more):", 
                               min = sales_res_char_assess$bathrooms_range[1],
                               max = 6,
                               value = c(sales_res_char_assess$bathrooms_range[1], 6),
                               step = 1,
                               post = "+"),
                   
                   # Hidden inputs for filters handled by profile selection
                   tags$div(style="display: none;",
                            selectInput("find_garage", "Garage:", 
                                        choices = c("Any", "Yes", "No")),
                            selectInput("find_finished_basement", "Finished Basement:", 
                                        choices = c("Any", "Yes", "No")),
                            selectInput("find_construction_grade", "Construction Grade:", 
                                        choices = c("Any", "Excellent", "Very Good", "Good", 
                                                    "Average", "Fair", "Low"))
                   ),
                   
                   sliderInput("find_year_built_range", "Year Built Range:", 
                               min = sales_res_char_assess$year_built_range[1],
                               max = sales_res_char_assess$year_built_range[2],
                               value = c(sales_res_char_assess$year_built_range[1], sales_res_char_assess$year_built_range[2]),
                               step = 1)
                 ),
                 
                 actionButton("clear_find", "Clear Pin & Results", 
                              class = "btn-secondary",
                              style = "width: 100%;")
        )
      ),
      
      # Market Analytics inputs
      conditionalPanel(
        condition = "input.tabs == 'Market Analytics'",
        div(class = "section-card",
            tags$h4("Selection Area", class = "section-header"),
            tags$div(class = "instruction-box",
                     style = "text-align: center;",
                     tags$i(class = "fas fa-map-marker-alt", 
                            style = "color: var(--primary-color); margin-right: 8px;"),
                     "Click on the map to analyze properties in a specific area"
            ),
            leafletOutput("map_analytics", height = "300px"),
            div(style = "margin-top: 15px;",
                sliderInput("analytics_radius", 
                            "Selection Radius:", 
                            min = 100,
                            max = 2000,
                            value = 1000,
                            step = 100,
                            post = " m")
            )
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        # Tab 1: Market Analytics
        tabPanel(
          "Market Analytics",
          fluidRow(
            # Full width for plots
            column(
              width = 12,
              fluidRow(
                # Top row with Price Trends and Seasonal Patterns
                column(
                  width = 6,
                  div(class = "section-card",
                      div(style = "display: flex; justify-content: space-between; align-items: center;",
                          h4("Price Trends", class = "section-header", style = "margin: 0;"),
                          div(style = "display: flex; gap: 10px;",
                              materialSwitch("price_metric", "Show Median", status = "primary", right = TRUE)
                          )
                      ),
                      plotOutput("price_trends_plot", height = "325px")
                  )
                ),
                column(
                  width = 6,
                  div(class = "section-card",
                      h4("Seasonal Price Patterns", class = "section-header"),
                      plotOutput("seasonal_plot", height = "325px")
                  )
                )
              ),
              # Bottom row with Price/Assessment plot
              fluidRow(
                column(
                  width = 12,
                  div(class = "section-card",
                      div(style = "display: flex; justify-content: space-between; align-items: center;",
                          h4("Price/Assessment", class = "section-header", style = "margin: 0;"),
                          div(style = "display: flex; gap: 10px;",
                              materialSwitch("heatmap_type", "Hexagonal", status = "primary", right = TRUE)
                          )
                      ),
                      plotOutput("heatmap_plot", height = "325px")
                  )
                )
              )
            )
          )
        ),
        # Tab 2: Sale Finder
        tabPanel(
          "Sale Finder",
          div(id = "loading_sale", class = "loading",
              div(class = "loading-wheel"),
              div(class = "loading-text", "Finding sales data...")),
          tags$div(
            class = "instruction-box",
            style = "text-align: center; border-left: none;",
            tags$i(class = "fas fa-map-marker-alt", style = "margin-right: 8px;"),
            "Click on the map to select an area or use the filters on the left"
          ),
          leafletOutput("map_sale", height = "700px")
        ),
        
        # Tab 3: Property Finder
        tabPanel(
          "Property Finder",
          fluidRow(
            column(12,
                   tags$div(class = "map-instruction instruction-box",
                            style = "text-align: center;",
                            tags$i(class = "fas fa-map-marker-alt", style = "color: var(--primary-color); margin-right: 8px;"),
                            "Click anywhere on the map to drop a pin and find nearby matching properties"
                   ),
                   div(id = "loading_find", class = "loading",
                       div(class = "loading-wheel"),
                       div(class = "loading-text", "Finding nearby properties...")),
                   leafletOutput("map_find", height = "700px")
            )
          )
        )
      )
    )
  )
)