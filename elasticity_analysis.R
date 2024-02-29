# elasticity_analysis.R

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(caret)
library(Metrics)

# Load data management and predictive analytics functions
source("data_management.R")
source("predictive_analytics.R")

# Function to perform elasticity analysis
perform_elasticity_analysis <- function(data, product_type) {
  # Ensure data is clean and of high quality
  data <- clean_data(data)
  
  # Filter data for the specific product type
  product_data <- data %>% filter(product == product_type)
  
  # Define outcome and predictors
  outcome <- "demand"
  predictors <- c("price")
  
  # Prepare the data for modeling
  model_data <- product_data %>%
    select(all_of(c(outcome, predictors))) %>%
    na.omit() # Removing missing values for simplicity
  
  # Split data into training and testing sets
  set.seed(123) # For reproducibility
  trainIndex <- createDataPartition(model_data[[outcome]], p = .8, 
                                    list = FALSE, 
                                    times = 1)
  trainData <- model_data[ trainIndex,]
  testData  <- model_data[-trainIndex,]
  
  # Train a linear regression model to estimate elasticity
  lm_model <- lm(as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))), 
                 data = trainData)
  
  # Extract the coefficient for price to estimate elasticity
  elasticity <- coef(lm_model)["price"]
  
  # Plotting the elasticity
  plot_data <- data.frame(price = trainData$price, demand = predict(lm_model, newdata = trainData))
  p <- ggplot(plot_data, aes(x = price, y = demand)) +
    geom_line() +
    geom_point(data = trainData, aes(x = price, y = demand), color = "red") +
    labs(title = paste("Price Elasticity of Demand for", product_type),
         x = "Price",
         y = "Demand") +
    annotate("text", x = Inf, y = Inf, label = paste("Elasticity:", round(elasticity, 2)), 
             hjust = 1.1, vjust = 2, size = 5, color = "blue")
  
  # Print the elasticity value
  print(paste("Estimated price elasticity of demand for", product_type, ":", round(elasticity, 2)))
  
  # Return the plot
  print(p)
}

# Example usage
# Assuming 'data' is already loaded and contains the necessary columns 'product', 'price', and 'demand'
# perform_elasticity_analysis(data, "ProductType1")
