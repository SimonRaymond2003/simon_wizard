# File: server.R
# Updated with pin-based Property Finder functionality

# Import required libraries in proper order to prevent masking warnings
library(shiny)
library(shinyjs)  # Load early to prevent masking warnings
library(leaflet)
library(sf)
library(promises)
library(future)
library(ggplot2)
library(htmltools)
library(base64enc)
library(dplyr)

# Server function
server <- function(input, output, session) {
  # Load helper functions
  source("global.R", local = TRUE)
  
  # Initialize reactive values
  find_it_coords <- reactiveVal(NULL)  # Store pin coordinates
  find_it_data <- reactiveVal(NULL)    # Store matching properties
  
  sale_coords <- reactiveVal(NULL)      # Store sale finder coordinates
  sale_data <- reactiveVal(NULL)        # Store sale finder results
  sale_selected_area <- reactiveVal(NULL)
  
  # Initialize maps
  output$map_sale <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -63.582687, lat = 44.651070, zoom = 12)
  })
  
  output$map_find <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = -63.582687, lat = 44.651070, zoom = 12)
  })
  
  # Property Finder pin dropping and auto-search
  observeEvent(input$map_find_click, {
    click <- input$map_find_click
    find_it_coords(c(click$lng, click$lat))
    # Initialize pin on map
    leafletProxy("map_find") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = click$lng,
        lat = click$lat,
        radius = 8,
        color = "#2F6B52",
        fillOpacity = 1,
        stroke = TRUE,
        weight = 2
      )

    # Automatically search for nearby properties
    shinyjs::show("loading_find")
    find_inputs <- reactiveValuesToList(input)
    pin_location <- c(click$lng, click$lat)
    
    future({
      # Filter properties based on criteria
      filtered_data <- sales_sf %>%
        filter(
          Sale.Price >= find_inputs$find_price_range[1],
          Sale.Price <= find_inputs$find_price_range[2],
          Square.Foot.Living.Area >= find_inputs$find_sqft_range[1],
          Square.Foot.Living.Area <= find_inputs$find_sqft_range[2],
          Bedrooms >= find_inputs$find_bedrooms_range[1],
          Bedrooms <= find_inputs$find_bedrooms_range[2],
          Bathrooms >= find_inputs$find_bathrooms_range[1],
          Bathrooms <= find_inputs$find_bathrooms_range[2],
          Year.Built >= find_inputs$find_year_built_range[1],
          Year.Built <= find_inputs$find_year_built_range[2]
        )
      
      if (find_inputs$find_garage != "Any") {
        filtered_data <- filtered_data %>%
          filter(Garage == if(find_inputs$find_garage == "Yes") "Y" else "N")
      }
      
      if (find_inputs$find_finished_basement != "Any") {
        filtered_data <- filtered_data %>%
          filter(Finished.Basement == if(find_inputs$find_finished_basement == "Yes") "Y" else "N")
      }
      
      if (find_inputs$find_construction_grade != "Any") {
        filtered_data <- filtered_data %>%
          filter(Construction.Grade == find_inputs$find_construction_grade)
      }
      
      # Calculate distances to pin for each property
      pin_point <- st_sfc(st_point(pin_location), crs = 4326)
      distances <- st_distance(filtered_data, pin_point)
      
      # Add distances to the data
      filtered_data$distance_to_pin <- as.numeric(distances)
      
      # Get the N closest properties
      filtered_data <- filtered_data %>%
        arrange(distance_to_pin) %>%
        head(find_inputs$nearby_matches)
      
      return(filtered_data)
    }, seed = TRUE) %...>% {
      find_it_data(.)
      shinyjs::hide("loading_find")
    } %...!% {
      shinyjs::hide("loading_find")
      showNotification("An error occurred while searching.", type = "error")
    }
  })
  
  # Update map display with markers and connecting lines
  observe({
    filtered <- find_it_data()
    pin_coords <- find_it_coords()
    
    if (is.null(filtered) || is.null(pin_coords)) {
      leafletProxy("map_find") %>%
        clearMarkers() %>%
        clearShapes()
      return()
    }
    
    # Create connecting lines
    lines <- lapply(1:nrow(filtered), function(i) {
      list(
        lng = c(pin_coords[1], filtered$i.X.Map.Coordinate[i]),
        lat = c(pin_coords[2], filtered$i.Y.Map.Coordinate[i])
      )
    })
    
    leafletProxy("map_find") %>%
      clearMarkers() %>%
      clearShapes() %>%
      # Add pin marker
      addCircleMarkers(
        lng = pin_coords[1],
        lat = pin_coords[2],
        radius = 8,
        color = "#2F6B52",
        fillOpacity = 1,
        stroke = TRUE,
        weight = 2
      ) %>%
      # Add property markers
      addCircleMarkers(
        lng = filtered$i.X.Map.Coordinate,
        lat = filtered$i.Y.Map.Coordinate,
        radius = 6,
        color = "#1E88E5",
        fillOpacity = 0.7,
        popup = paste(
          "<div style='max-height: 300px; overflow-y: auto;'>",
          "<b>Price & Location</b><br>",
          "Sale Price: $", format(filtered$Sale.Price, big.mark = ","), "<br>",
          "Distance: ", round(filtered$distance_to_pin), " meters<br>",
          "City:", filtered$Civic.City.Name, "<br>",
          "Postal Code:", filtered$Postal.Codes, "<br>",
          "<br><b>Property Details</b><br>",
          "Year Built:", filtered$Year.Built, "<br>",
          "Square Feet:", format(filtered$Square.Foot.Living.Area, big.mark = ","), "<br>",
          "Bedrooms:", filtered$Bedrooms, "<br>",
          "Bathrooms:", filtered$Bathrooms, "<br>",
          "Construction Grade:", filtered$Construction.Grade, "<br>",
          "Garage:", if_else(filtered$Garage == "Y", "Yes", "No"), "<br>",
          "Finished Basement:", if_else(filtered$Finished.Basement == "Y", "Yes", "No"), "<br>",
          "<br><b>Links</b><br>",
          "<a href='https://www.google.com/maps/search/?api=1&query=", 
          filtered$i.Y.Map.Coordinate, ",", filtered$i.X.Map.Coordinate, 
          "' target='_blank'>View on Google Maps</a><br>",
          "<a href='https://www.google.com/maps/@?api=1&map_action=pano&viewpoint=", 
          filtered$i.Y.Map.Coordinate, ",", filtered$i.X.Map.Coordinate, 
          "' target='_blank'>Open Street View</a>",
          "</div>"
        )
      )
    
    # Add connecting lines
    for (line in lines) {
      leafletProxy("map_find") %>%
        addPolylines(
          lng = line$lng,
          lat = line$lat,
          color = "#1E88E5",
          weight = 1,
          opacity = 0.6,
          dashArray = "4"
        )
    }
    
    if (nrow(filtered) > 0) {
      # Calculate bounds including both pin and properties
      lng_bounds <- range(c(pin_coords[1], filtered$i.X.Map.Coordinate))
      lat_bounds <- range(c(pin_coords[2], filtered$i.Y.Map.Coordinate))
      
      # Add padding to bounds
      lng_pad <- diff(lng_bounds) * 0.1
      lat_pad <- diff(lat_bounds) * 0.1
      
      leafletProxy("map_find") %>%
        fitBounds(
          lng1 = lng_bounds[1] - lng_pad,
          lat1 = lat_bounds[1] - lat_pad,
          lng2 = lng_bounds[2] + lng_pad,
          lat2 = lat_bounds[2] + lat_pad
        )
    }
  })
  
  # Handle buyer profile selection
  observeEvent(input$buyer_profile, {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    if (input$buyer_profile == "young_prof") {
      # Young Professional/Couple - Modern, compact living
      updateSliderInput(session, "nearby_matches", value = 100)
      updateSliderInput(session, "find_price_range", 
                       value = c(300000, 600000))
      updateSliderInput(session, "find_sqft_range", 
                       value = c(800, 1500))
      updateSliderInput(session, "find_bedrooms_range", 
                       value = c(1, 2))
      updateSliderInput(session, "find_bathrooms_range", 
                       value = c(1, 2))
      updateSliderInput(session, "find_year_built_range", 
                       value = c(current_year - 20, current_year))
      updateSelectInput(session, "find_garage", selected = "Any")
      updateSelectInput(session, "find_finished_basement", selected = "Any")
      updateSelectInput(session, "find_construction_grade", 
                       selected = "Any")
      
    } else if (input$buyer_profile == "new_family") {
      # New Family - Modest but growing space needs
      updateSliderInput(session, "nearby_matches", value = 100)
      updateSliderInput(session, "find_price_range", 
                       value = c(350000, 700000))
      updateSliderInput(session, "find_sqft_range", 
                       value = c(1200, 2000))
      updateSliderInput(session, "find_bedrooms_range", 
                       value = c(2, 3))
      updateSliderInput(session, "find_bathrooms_range", 
                       value = c(1, 3))
      updateSliderInput(session, "find_year_built_range", 
                       value = c(current_year - 30, current_year))
      updateSelectInput(session, "find_garage", selected = "Yes")
      updateSelectInput(session, "find_finished_basement", selected = "Any")
      updateSelectInput(session, "find_construction_grade", 
                       selected = "Any")
      
    } else if (input$buyer_profile == "family") {
      # Family with Children - Spacious with amenities
      updateSliderInput(session, "nearby_matches", value = 100)
      updateSliderInput(session, "find_price_range", 
                       value = c(450000, 1000000))
      updateSliderInput(session, "find_sqft_range", 
                       value = c(1800, 3500))
      updateSliderInput(session, "find_bedrooms_range", 
                       value = c(3, 5))
      updateSliderInput(session, "find_bathrooms_range", 
                       value = c(2, 4))
      updateSliderInput(session, "find_year_built_range", 
                       value = c(current_year - 40, current_year))
      updateSelectInput(session, "find_garage", selected = "Yes")
      updateSelectInput(session, "find_finished_basement", selected = "Yes")
      updateSelectInput(session, "find_construction_grade", 
                       selected = "Any")
      
    } else if (input$buyer_profile == "retiree") {
      # Retiree - Comfortable, low maintenance
      updateSliderInput(session, "nearby_matches", value = 100)
      updateSliderInput(session, "find_price_range", 
                       value = c(250000, 600000))
      updateSliderInput(session, "find_sqft_range", 
                       value = c(1000, 2000))
      updateSliderInput(session, "find_bedrooms_range", 
                       value = c(1, 2))
      updateSliderInput(session, "find_bathrooms_range", 
                       value = c(1, 2))
      updateSliderInput(session, "find_year_built_range", 
                       value = c(current_year - 25, current_year))
      updateSelectInput(session, "find_garage", selected = "Yes")
      updateSelectInput(session, "find_finished_basement", selected = "No")
      updateSelectInput(session, "find_construction_grade", 
                       selected = "Any")
    }
  })
  
  # Clear property finder
  observeEvent(input$clear_find, {
    find_it_coords(NULL)
    find_it_data(NULL)
    
    leafletProxy("map_find") %>%
      clearMarkers() %>%
      clearShapes() %>%
      setView(lng = -63.582687, lat = 44.651070, zoom = 12)
    
    updateRadioButtons(session, "buyer_profile", selected = character(0))
    updateSliderInput(session, "nearby_matches", value = 100)
    updateSliderInput(session, "find_price_range", 
                     value = c(sales_res_char_assess$price_range[1], 
                             sales_res_char_assess$price_range[2]))
    updateSliderInput(session, "find_sqft_range", 
                     value = c(sales_res_char_assess$sqft_range[1], 
                             sales_res_char_assess$sqft_range[2]))
    updateSliderInput(session, "find_bedrooms_range", 
                     value = c(sales_res_char_assess$bedrooms_range[1], 
                             sales_res_char_assess$bedrooms_range[2]))
    updateSliderInput(session, "find_bathrooms_range", 
                     value = c(sales_res_char_assess$bathrooms_range[1], 
                             sales_res_char_assess$bathrooms_range[2]))
    updateSliderInput(session, "find_year_built_range", 
                     value = c(sales_res_char_assess$year_built_range[1], 
                             sales_res_char_assess$year_built_range[2]))
    updateSelectInput(session, "find_garage", selected = "Any")
    updateSelectInput(session, "find_finished_basement", selected = "Any")
    updateSelectInput(session, "find_construction_grade", selected = "Any")
  })
  
  # Sale finder functionality
  observeEvent(input$map_sale_click, {
    if (input$sale_filter_type == "Area") {
      click <- input$map_sale_click
      sale_coords(c(click$lng, click$lat))
      
      sale_selected_area(list(
        center = c(click$lng, click$lat),
        radius = input$sale_radius
      ))
      
      leafletProxy("map_sale") %>%
        clearShapes() %>%
        addCircles(
          lng = click$lng, 
          lat = click$lat,
          radius = input$sale_radius,
          color = "#1E88E5",
          fill = TRUE, 
          fillOpacity = 0.2
        )
    }
  })
  
  # Find sales
  observeEvent(input$find_sales, {
    shinyjs::show("loading_sale")
    
    sale_inputs <- reactiveValuesToList(input)
    area <- sale_selected_area()
    
    future({
      filtered_data <- sales_sf %>%
        filter(
          Sale.Year >= sale_inputs$sale_year_range[1],
          Sale.Year <= sale_inputs$sale_year_range[2]
        )
      
      if (sale_inputs$sale_filter_type == "Area" && !is.null(area)) {
        point <- st_sfc(st_point(area$center), crs = 4326)
        buffer <- st_buffer(point, dist = area$radius)
        indices <- st_intersects(filtered_data, buffer, sparse = FALSE)
        filtered_data <- filtered_data[apply(indices, 1, any), ]
      } else if (sale_inputs$sale_filter_type == "Street") {
        filtered_data <- filtered_data %>%
          filter(grepl(sale_inputs$sale_street_name, Civic.Street.Name, ignore.case = TRUE))
      } else if (sale_inputs$sale_filter_type == "Postal Code") {
        filtered_data <- filtered_data %>%
          filter(grepl(sale_inputs$sale_postal_code, Postal.Codes, ignore.case = TRUE))
      } else if (sale_inputs$sale_filter_type == "City") {
        filtered_data <- filtered_data %>%
          filter(grepl(sale_inputs$sale_city_name, Civic.City.Name, ignore.case = TRUE))
      } else if (sale_inputs$sale_filter_type == "3-digit Postal Code") {
        filtered_data <- filtered_data %>%
          filter(grepl(sale_inputs$sale_postal_code_3digit, FirstThreeDigits, ignore.case = TRUE))
      }
      
      return(filtered_data)
    }, seed = TRUE) %...>% {
      sale_data(.)
      shinyjs::hide("loading_sale")
    } %...!% {
      shinyjs::hide("loading_sale")
      showNotification("An error occurred while searching for sales.", type = "error")
    }
  })
  
  # Update sale finder markers
  observe({
    filtered <- sale_data()
    
    if (is.null(filtered)) {
      leafletProxy("map_sale") %>%
        clearMarkers()
      return()
    }
    
    leafletProxy("map_sale") %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = filtered$i.X.Map.Coordinate,
        lat = filtered$i.Y.Map.Coordinate,
        radius = 4,
        color = "#1E88E5",
        fillOpacity = 0.7,
        popup = paste(
          "<div style='max-height: 300px; overflow-y: auto;'>",
          "<b>Sale Information</b><br>",
          "Year:", filtered$Sale.Year, "<br>",
          "Sale Price: $", format(filtered$Sale.Price, big.mark = ","), "<br>",
          "Assessed Value: $", format(filtered$Assessed.Value, big.mark = ","), "<br>",
          "<br><b>Location</b><br>",
          "Street:", filtered$Civic.Street.Name, "<br>",
          "City:", filtered$Civic.City.Name, "<br>",
          "Postal Code:", filtered$Postal.Codes, "<br>",
          "<br><b>Property Details</b><br>",
          "Living Units:", filtered$Living.Units, "<br>",
          "Year Built:", filtered$Year.Built, "<br>",
          "Square Feet:", format(filtered$Square.Foot.Living.Area, big.mark = ","), " sq ft<br>",
          "Style:", filtered$Style, "<br>",
          "Bedrooms:", filtered$Bedrooms, "<br>",
          "Bathrooms:", filtered$Bathrooms, "<br>",
          "<br><b>Links</b><br>",
          "<a href='https://www.google.com/maps/search/?api=1&query=", 
          filtered$i.Y.Map.Coordinate, ",", filtered$i.X.Map.Coordinate, 
          "' target='_blank'>View on Google Maps</a><br>",
          "<a href='https://www.google.com/maps/@?api=1&map_action=pano&viewpoint=", 
          filtered$i.Y.Map.Coordinate, ",", filtered$i.X.Map.Coordinate, 
          "' target='_blank'>Open Street View</a>",
          "</div>"
        )
      )
    
    if (nrow(filtered) > 0) {
      leafletProxy("map_sale") %>%
        fitBounds(
          lng1 = min(filtered$i.X.Map.Coordinate), lat1 = min(filtered$i.Y.Map.Coordinate),
          lng2 = max(filtered$i.X.Map.Coordinate), lat2 = max(filtered$i.Y.Map.Coordinate)
        )
    }
  })
  
  # Clear sale finder
  observeEvent(input$clear_sale, {
    sale_coords(NULL)
    sale_data(NULL)
    sale_selected_area(NULL)
    
    leafletProxy("map_sale") %>%
      clearMarkers() %>%
      clearShapes() %>%
      setView(lng = -63.582687, lat = 44.651070, zoom = 12)
    
    updateSliderInput(session, "sale_year_range", value = c(2020, 2024))
    updateSelectInput(session, "sale_filter_type", selected = "Area")
    updateSliderInput(session, "sale_radius", value = 200)
    updateTextInput(session, "sale_city_name", value = "")
    updateTextInput(session, "sale_street_name", value = "")
    updateTextInput(session, "sale_postal_code", value = "")
    updateTextInput(session, "sale_postal_code_3digit", value = "")
  })
}
