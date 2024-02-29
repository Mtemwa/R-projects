# portfolio_insights.R

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# Load data management functions
source("data_management.R")

# Function to generate portfolio performance insights
generate_portfolio_insights <- function() {
  # Fetch portfolio data from the database
  query <- "SELECT * FROM portfolio_data" # Example query, adjust based on actual data structure
  portfolio_data <- fetch_data(query)
  
  # Clean and preprocess data
  portfolio_data <- clean_data(portfolio_data)
  
  # Calculate key performance indicators (KPIs)
  portfolio_performance <- portfolio_data %>%
    summarise(
      total_premium_income = sum(premium_income),
      average_loss_ratio = mean(loss_ratio),
      average_expense_ratio = mean(expense_ratio)
    )
  
  # Return the calculated KPIs
  return(portfolio_performance)
}

# Function to create a portfolio performance dashboard
create_portfolio_dashboard <- function(portfolio_performance) {
  ui <- fluidPage(
    titlePanel("Portfolio Performance Dashboard"),
    sidebarLayout(
      sidebarPanel(
        h3("Portfolio KPIs"),
        verbatimTextOutput("kpiDisplay")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Performance Overview", plotlyOutput("performancePlot")),
          tabPanel("Data Table", DTOutput("dataTable"))
        )
      )
    )
  )
  
  server <- function(input, output) {
    output$kpiDisplay <- renderPrint({
      portfolio_performance
    })
    
    output$performancePlot <- renderPlotly({
      # Example plot - adjust based on actual KPIs and desired visualizations
      p <- ggplot(portfolio_performance, aes(x = total_premium_income, y = average_loss_ratio)) +
        geom_point() +
        theme_minimal() +
        labs(title = "Portfolio Performance Overview", x = "Total Premium Income", y = "Average Loss Ratio")
      ggplotly(p)
    })
    
    output$dataTable <- renderDT({
      datatable(portfolio_data, options = list(pageLength = 5))
    })
  }
  
  # Run the Shiny app
  shinyApp(ui = ui, server = server)
}

# Example usage
portfolio_performance <- generate_portfolio_insights()
create_portfolio_dashboard(portfolio_performance)
