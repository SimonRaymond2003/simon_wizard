# File: global.R
# Install required packages if missing
required_packages <- c("shiny", "dplyr", "httr", "jsonlite", "memoise", 
                      "digest", "data.table", "sf", "dotenv", "hexbin",
                      "viridis", "lubridate", "scales", "shinyWidgets")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required libraries
library(shiny)
library(dplyr)
library(httr)
library(jsonlite)
library(memoise)
library(digest)
library(data.table)
library(sf)
library(dotenv)
library(shinyWidgets)
library(hexbin)
library(viridis)
library(lubridate)
library(scales)

# Load environment variables
if (file.exists(".env")) {
  load_dot_env(file = ".env")
  if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
    stop("ANTHROPIC_API_KEY not found in .env file")
  }
  
  # Print API key status (masked for security)
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key != "") {
    message("ANTHROPIC_API_KEY found: ", substr(api_key, 1, 8), "...")
  } else {
    stop(".env file found but ANTHROPIC_API_KEY is empty")
  }
} else {
  stop(".env file not found in ", getwd())
}


# Load and process data
sales_file <- "sales_res_char_assess.csv.gz"
if (!file.exists(sales_file)) {
  stop("Required data file not found: ", sales_file)
}

# Read data with data.table for better performance
sales_dt <- fread(sales_file)

# Clean and transform data
sales_dt[, `:=`(
  Sale.Year = as.integer(format(as.Date(Sale.Date), "%Y")),
  X = i.X.Map.Coordinate,
  Y = i.Y.Map.Coordinate,
  FirstThreeDigits = substr(Postal.Codes, 1, 3)
)]

# Convert to spatial features
valid_coords <- !is.na(sales_dt$X) & !is.na(sales_dt$Y)
sales_sf <- st_as_sf(
  sales_dt[valid_coords],
  coords = c("X", "Y"),
  crs = 4326,
  remove = FALSE
)

# Create global min/max values for UI controls
sales_res_char_assess <- list(
  year_range = range(sales_dt$Sale.Year, na.rm = TRUE),
  price_range = range(sales_dt$Sale.Price, na.rm = TRUE),
  sqft_range = range(sales_dt$Square.Foot.Living.Area, na.rm = TRUE),
  bedrooms_range = range(sales_dt$Bedrooms, na.rm = TRUE),
  bathrooms_range = range(sales_dt$Bathrooms, na.rm = TRUE),
  year_built_range = range(sales_dt$Year.Built, na.rm = TRUE)
)

# Simple API function that worked before
call_anthropic_api <- memoise::memoise(function(system_prompt, user_content) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") return(list(error = TRUE, message = "Error: ANTHROPIC_API_KEY not set"))
  
  tryCatch({
    response <- httr::POST(
      url = "https://api.anthropic.com/v1/messages",
      httr::add_headers(
        "Content-Type" = "application/json",
        "x-api-key" = api_key,
        "anthropic-version" = "2024-01-01"
      ),
      body = list(
        model = "claude-3-sonnet-20240229",
        system = system_prompt,
        messages = list(list(
          role = "user",
          content = user_content
        )),
        max_tokens = 1024,
        temperature = 0.3
      ),
      encode = "json"
    )
    
    if (httr::status_code(response) != 200) {
      return(list(error = TRUE, message = "API request failed"))
    }
    
    content <- httr::content(response, "parsed")
    if (!is.null(content$error)) {
      return(list(error = TRUE, message = paste("API Error:", content$error)))
    }
    # Claude 3 Messages API Response Format
    if (!is.null(content)) {
      # Debug output
      message("API Response structure:")
      print(str(content))
      
      # Content is in content field of the response for Claude 3
      if (!is.null(content$message) && 
          !is.null(content$message$content) && 
          length(content$message$content) > 0 &&
          !is.null(content$message$content[[1]]$text)) {
        # Get the assistant's response text
        text <- trimws(content$message$content[[1]]$text)
        list(error = FALSE, text = text)
      } else {
        # Print response structure for debugging
        message("API Response structure:")
        print(str(content))
        return(list(error = TRUE, message = "Could not extract text from API response"))
      }
    } else {
      return(list(error = TRUE, message = "Empty API response"))
    }
    
  }, error = function(e) {
    list(error = TRUE, message = paste("API Error:", e$message))
  })
})

# Generate HTML report with error handling
generate_report_html <- function(query, analysis_result, plot_path, timestamp) {
  tryCatch({
    # Convert special characters to HTML entities
    query <- htmltools::htmlEscape(query)
    # Convert analysis text to HTML with proper formatting
    analysis_text_formatted <- analysis_result %>%
      htmltools::htmlEscape() %>%
      gsub("\n", "<br>", .) %>%  # Convert newlines to <br>
      # Style headers
      gsub("^(#+ .+)$", "<h3 style='color: #2F6B52; margin-top: 1.5em;'>\\1</h3>", ., perl = TRUE) %>%
      # Style lists
      gsub("^- (.+)$", "<li>\\1</li>", ., perl = TRUE) %>%
      gsub("(<li>.+</li>\n?)+", "<ul style='margin: 1em 0;'>\\0</ul>", ., perl = TRUE)
    
    # Read and encode plot file
    plot_base64 <- tryCatch({
      if (file.exists(plot_path)) {
        base64enc::base64encode(plot_path)
      } else {
        message("Plot file not found:", plot_path)
        NULL
      }
    }, error = function(e) {
      message("Error encoding plot:", e$message)
      NULL
    })
    
    # Generate HTML
    html <- paste0(
      '<div style="font-family: \'Inter\', sans-serif; max-width: 800px; margin: 0 auto;">',
      '<h2 style="color: #2F6B52;">Market Analysis Report</h2>',
      sprintf('<p style="color: #666;">Generated on: %s</p>', format(timestamp, "%B %d, %Y at %I:%M %p")),
      '<div style="background: #f8fafc; border-left: 4px solid #2F6B52; padding: 15px; margin: 20px 0;">',
      sprintf('<strong>Query:</strong> %s', query),
      '</div>',
      '<div style="margin: 20px 0;">',
      analysis_text_formatted,
      '</div>'
    )
    
    # Add plot if available
    if (!is.null(plot_base64)) {
      html <- paste0(
        html,
        '<div style="margin: 20px 0;">',
        sprintf('<img src="data:image/png;base64,%s" style="max-width: 100%%; height: auto;">', 
                plot_base64),
        '</div>'
      )
    }
    
    html <- paste0(html, '</div>')
    return(html)
  }, error = function(e) {
    # Return error message as HTML if something goes wrong
    paste0(
      '<div style="color: #e53e3e; padding: 20px; background: #fff5f5; border-left: 4px solid #e53e3e;">',
      '<strong>Error generating report:</strong><br>',
      htmltools::htmlEscape(e$message),
      '</div>'
    )
  })
}

# Agent 1: Query Interpreter
interpret_query <- function(query, data_structure, sample_data) {
  system_prompt <- "You are an expert in real estate market analysis. Your task is to interpret and clarify the user's query about housing data. Consider temporal trends, price patterns, and location-based insights. Respond with a clear, focused analysis question."
  
  # Ensure inputs are single strings
  data_structure_str <- paste(data_structure, collapse = "\n")
  sample_data_str <- paste(sample_data, collapse = "\n")
  
  user_content <- paste0(
    "Query: ", query, "\n\n",
    "Data Structure:\n", data_structure_str, "\n\n",
    "Sample Data:\n", sample_data_str
  )
  
  result <- call_anthropic_api(system_prompt, user_content)
  if (result$error) stop(result$message)
  result$text
}

# Agent 2: Method Selector
select_methods <- function(interpreted_query, data_structure, sample_data) {
  system_prompt <- paste(
    "You are a real estate data scientist analyzing Nova Scotia housing data.",
    "Based on the interpreted query, suggest specific analytical methods and relevant columns.",
    "You must choose methods that work with the available data structure.",
    "Focus on clear, actionable analysis approaches.",
    "Consider basic statistics, trends over time, and price distributions.",
    sep = "\n"
  )
  
  # Ensure inputs are single strings
  data_structure_str <- paste(data_structure, collapse = "\n")
  sample_data_str <- paste(sample_data, collapse = "\n")
  
  user_content <- paste0(
    "Interpreted Query: ", interpreted_query, "\n\n",
    "Data Structure:\n", data_structure_str, "\n\n",
    "Sample Data:\n", sample_data_str
  )
  
  result <- call_anthropic_api(system_prompt, user_content)
  if (result$error) stop(result$message)
  result$text
}

# Agent 3: Analysis Creator
create_analysis <- function(methods, data, data_structure, sample_data) {
  system_prompt <- paste(
    "You are a real estate market analyst for Nova Scotia.",
    "Analyze the data to provide clear insights about property values and market trends.",
    "Use plain language to explain findings.",
    "Focus on practical implications for understanding the local market.",
    sep = "\n"
  )
  
  # Calculate key statistics
  stats <- list(
    total = nrow(data),
    year_range = range(data$Sale.Year, na.rm = TRUE),
    price_stats = list(
      median = median(data$Sale.Price, na.rm = TRUE),
      mean = mean(data$Sale.Price, na.rm = TRUE),
      recent_trend = median(data$Sale.Price[data$Sale.Year >= max(data$Sale.Year) - 2], na.rm = TRUE) /
        median(data$Sale.Price[data$Sale.Year <= min(data$Sale.Year) + 2], na.rm = TRUE)
    )
  )
  
  # Format data summary
  data_summary <- paste(
    "Market Analysis Summary:",
    sprintf("- Total properties analyzed: %d", stats$total),
    sprintf("- Time period: %d to %d", stats$year_range[1], stats$year_range[2]),
    sprintf("- Median price: $%s", format(stats$price_stats$median, big.mark = ",")),
    sprintf("- Mean price: $%s", format(stats$price_stats$mean, big.mark = ",")),
    sprintf("- Price trend (recent vs early): %.1fx", stats$price_stats$recent_trend),
    sep = "\n"
  )
  
  user_content <- paste0(
    "Analysis Methods:\n", methods, "\n\n",
    "Data Summary:\n", data_summary, "\n\n",
    "Data Structure:\n", paste(data_structure, collapse = "\n"), "\n\n",
    "Sample Data:\n", paste(sample_data, collapse = "\n")
  )
  
  result <- call_anthropic_api(system_prompt, user_content)
  if (result$error) stop(result$message)
  result$text
}

# Agent 4: Plot Creator
create_plot <- function(analysis_text, data_structure, sample_data) {
  system_prompt <- paste(
    "You are a data visualization expert.",
    "Create ggplot2 code to visualize Nova Scotia housing market trends.",
    "Focus on creating clear, informative visualizations that support the analysis.",
    "Use theme_minimal() for consistent styling.",
    "Return ONLY the complete ggplot2 code with no other text.",
    "Available aesthetics: color='#2F6B52', fill='#2F6B52'",
    sep = "\n"
  )
  
  user_content <- paste0(
    "Analysis:\n", analysis_text, "\n\n",
    "Data Structure:\n", paste(data_structure, collapse = "\n"), "\n\n",
    "Sample Data:\n", paste(sample_data, collapse = "\n")
  )
  
  result <- call_anthropic_api(system_prompt, user_content)
  if (result$error) stop(result$message)
  result$text
}

# Execute Plot Helper
execute_plot <- function(plot_code, data) {
  tryCatch({
    eval(parse(text = plot_code), envir = list2env(list(filtered_df = data)))
  }, error = function(e) {
    ggplot() + 
      annotate("text", x = 1, y = 1, 
               label = paste("Error creating plot:", e$message)) +
      theme_void()
  })
}
