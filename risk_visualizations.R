# risk_visualizations.R

# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Load data management and predictive analytics functions
source("data_management.R")
source("predictive_analytics.R")

# Function to create dynamic visualizations for risk exposure
visualize_risk_exposure <- function(data, product_type) {
  # Filter data for the specific product type
  product_data <- data %>% filter(product == product_type)
  
  # Generate a basic risk exposure plot
  exposure_plot <- ggplot(product_data, aes(x = risk_factor, y = exposure_value, fill = risk_level)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Risk Exposure for", product_type, "Insurance"), x = "Risk Factor", y = "Exposure Value") +
    scale_fill_manual(values = c("Low" = "green", "Medium" = "yellow", "High" = "red"))
  
  # Convert ggplot object to plotly for interactivity
  interactive_exposure_plot <- ggplotly(exposure_plot)
  
  return(interactive_exposure_plot)
}

# Function to create dynamic visualizations for mitigation strategies
visualize_mitigation_strategies <- function(data, strategy_type) {
  # Filter data for the specific mitigation strategy
  strategy_data <- data %>% filter(strategy == strategy_type)
  
  # Generate a basic mitigation strategy plot
  strategy_plot <- ggplot(strategy_data, aes(x = strategy_effectiveness, y = strategy_cost, color = strategy_type)) +
    geom_point() +
    theme_minimal() +
    labs(title = paste("Mitigation Strategy Effectiveness vs. Cost for", strategy_type), x = "Effectiveness", y = "Cost") +
    scale_color_manual(values = c("Preventive" = "blue", "Corrective" = "orange"))
  
  # Convert ggplot object to plotly for interactivity
  interactive_strategy_plot <- ggplotly(strategy_plot)
  
  return(interactive_strategy_plot)
}

# Example usage
# Assuming 'insurance_data' is a dataframe loaded and cleaned through data_management.R functions
# product_type_example <- "auto"
# strategy_type_example <- "Preventive"

# exposure_plot <- visualize_risk_exposure(insurance_data, product_type_example)
# strategy_plot <- visualize_mitigation_strategies(insurance_data, strategy_type_example)

# Note: To display these plots in a Shiny app, you would use the plotlyOutput and renderPlotly functions within your UI and server definitions, respectively.
