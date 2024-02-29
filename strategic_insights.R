# strategic_insights.R

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)
library(lubridate)
library(httr)
library(jsonlite)

# Load data management and analytics functions
source("data_management.R")
source("risk_models.R")
source("predictive_analytics.R")
source("portfolio_insights.R")
source("benchmarking_tools.R")
source("claims_forecasting.R")
source("pricing_models.R")
source("capital_requirements.R")
source("compliance_alerts.R")

# Function to integrate insights from various modules for strategic decision-making
generate_strategic_insights <- function() {
  # Fetch and prepare data from various modules
  portfolio_performance <- generate_portfolio_insights()
  benchmarks <- fetch_benchmarks()
  claims_forecast <- forecast_claims(data, product_type = "example_product", forecast_horizon = 12)
  pricing_strategy <- fetch_pricing_models()
  capital_info <- fetch_capital_requirements()
  
  # Combine insights into a comprehensive dataframe
  strategic_insights_df <- data.frame(
    "Metric" = c("Total Premium Income", "Average Loss Ratio", "Claims Forecast", "Pricing Strategy", "Capital Requirement"),
    "Value" = c(
      portfolio_performance$total_premium_income,
      portfolio_performance$average_loss_ratio,
      claims_forecast$forecast_value, # Assuming forecast_claims returns a dataframe with forecast_value
      pricing_strategy$selected_strategy, # Assuming fetch_pricing_models returns a dataframe with selected_strategy
      capital_info$required_capital # Assuming fetch_capital_requirements returns a dataframe with required_capital
    )
  )
  
  return(strategic_insights_df)
}

# UI for Strategic Insights Dashboard
ui <- fluidPage(
  titlePanel("Strategic Insights Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Strategic Metrics"),
      verbatimTextOutput("metricsDisplay")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Strategic Overview", plotlyOutput("strategicPlot")),
        tabPanel("Detailed Insights", DTOutput("insightsTable"))
      )
    )
  )
)

# Server logic for Strategic Insights Dashboard
server <- function(input, output) {
  strategic_data <- reactive({ generate_strategic_insights() })
  
  output$metricsDisplay <- renderPrint({
    strategic_data()
  })
  
  output$strategicPlot <- renderPlotly({
    df <- strategic_data()
    plot_ly(df, x = ~Metric, y = ~Value, type = 'bar', marker = list(color = 'rgba(50, 171, 96, 0.7)'))
  })
  
  output$insightsTable <- renderDT({
    datatable(strategic_data(), options = list(pageLength = 5))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
