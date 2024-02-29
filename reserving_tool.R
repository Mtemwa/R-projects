# reserving_tool.R

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Load data management, predictive analytics, and claims forecasting functions
source("data_management.R")
source("predictive_analytics.R")
source("claims_forecasting.R")

# Function to automate the calculation of reserves
automate_reserves_calculation <- function(data, product_types, forecast_horizon, reserve_multiplier) {
  # Initialize an empty data frame to store reserve calculations
  reserves_summary <- data.frame(product_type = character(),
                                 forecasted_claims = numeric(),
                                 calculated_reserves = numeric(),
                                 stringsAsFactors = FALSE)
  
  # Loop through each product type
  for (product_type in product_types) {
    # Forecast claims for the product type
    forecasted_claims <- forecast_claims(data, product_type, forecast_horizon)
    
    # Calculate reserves based on the forecasted claims
    calculated_reserves <- calculate_reserves(forecasted_claims, reserve_multiplier)
    
    # Append the results to the reserves summary data frame
    reserves_summary <- rbind(reserves_summary, data.frame(product_type = product_type,
                                                           forecasted_claims = sum(forecasted_claims),
                                                           calculated_reserves = calculated_reserves))
  }
  
  return(reserves_summary)
}

# Function for scenario analysis on reserves
perform_scenario_analysis <- function(data, product_types, forecast_horizon, reserve_multipliers) {
  # Initialize an empty list to store scenario analysis results
  scenario_results <- list()
  
  # Loop through each reserve multiplier scenario
  for (reserve_multiplier in reserve_multipliers) {
    # Calculate reserves for each product type under the current scenario
    scenario_reserves <- automate_reserves_calculation(data, product_types, forecast_horizon, reserve_multiplier)
    
    # Add the scenario results to the list
    scenario_results[[paste("Multiplier", reserve_multiplier)]] <- scenario_reserves
  }
  
  return(scenario_results)
}

# Function to visualize scenario analysis results
visualize_scenario_analysis <- function(scenario_results) {
  # Combine all scenario results into a single data frame
  all_scenarios_df <- do.call(rbind, lapply(names(scenario_results), function(name) {
    scenario_df <- scenario_results[[name]]
    scenario_df$scenario <- name
    return(scenario_df)
  }))
  
  # Plot calculated reserves for each product type across scenarios
  ggplot(all_scenarios_df, aes(x = product_type, y = calculated_reserves, fill = scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(title = "Reserve Calculations Across Different Scenarios",
         x = "Product Type",
         y = "Calculated Reserves",
         fill = "Scenario")
}

# Example usage
# Assuming 'insurance_data' is your dataset, 'product_types' is defined, and 'reserve_multipliers' is a vector of multipliers
# scenario_results <- perform_scenario_analysis(insurance_data, c("auto", "home"), 365, c(1.2, 1.5, 1.8))
# visualize_scenario_analysis(scenario_results)
