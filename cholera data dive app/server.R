# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(bslib)
library(plotly)

# Load data
data <- read.csv("cholera joined data.csv")

# Convert Year to numeric if necessary
data$Year <- as.numeric(data$Year)

function(input, output) {
  
  # Value Boxes
  output$total_cases <- renderValueBox({
    valueBox(
      value = sum(data$Number_cases, na.rm = TRUE),
      subtitle = "Total Cholera Cases",
      icon = icon("thermometer-half"),
      color = "aqua"
    )
  })
  
  output$total_deaths <- renderValueBox({
    valueBox(
      value = sum(data$Number_deaths, na.rm = TRUE),
      subtitle = "Total Cholera Deaths",
      icon = icon("heart-broken"),
      color = "red"
    )
  })
  
  output$avg_fatality <- renderValueBox({
    valueBox(
      value = round(mean(data$Case_fatality, na.rm = TRUE), 2),
      subtitle = "Avg Case Fatality Rate",
      icon = icon("percent"),
      color = "yellow"
    )
  })
  
  # Overall trend plot
  output$overall_trend_plot <- renderPlotly({
    overall_trend <- data %>%
      group_by(Year) %>%
      summarise(total_cases = sum(Number_cases, na.rm = TRUE),
                total_deaths = sum(Number_deaths, na.rm = TRUE))
    
    p <- ggplot(overall_trend, aes(x = Year)) +
      geom_line(aes(y = total_cases, color = "Cases"), size = 1) +
      geom_line(aes(y = total_deaths, color = "Deaths"), size = 1) +
      labs(title = "Cholera Cases & Deaths Over Time", y = "Count") +
      scale_color_manual(values = c("Cases" = "blue", "Deaths" = "red"))
    
    ggplotly(p)
  })
  
  # Country-specific plot
  output$country_plot <- renderPlotly({
    req(input$country, input$yearRange)  # Ensure inputs are available
    country_data <- data %>%
      filter(Country == input$country & Year >= input$yearRange[1] & Year <= input$yearRange[2])
    
    p <- ggplot(country_data, aes(x = Year)) +
      geom_bar(aes(y = Number_cases, fill = "Cases"), stat = "identity", position = "dodge") +
      geom_bar(aes(y = Number_deaths, fill = "Deaths"), stat = "identity", position = "dodge") +
      labs(title = paste("Cholera in", input$country), y = "Count") +
      scale_fill_manual(values = c("Cases" = "blue", "Deaths" = "red"))
    
    ggplotly(p)
  })
  
  # Yearly cases trend
  output$yearly_cases_plot <- renderPlotly({
    yearly_cases <- data %>%
      group_by(Year) %>%
      summarise(total_cases = sum(Number_cases, na.rm = TRUE))
    
    p <- ggplot(yearly_cases, aes(x = Year, y = total_cases)) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Yearly Cholera Cases", y = "Total Cases")
    
    ggplotly(p)
  })
  
  # Yearly deaths trend
  output$yearly_deaths_plot <- renderPlotly({
    yearly_deaths <- data %>%
      group_by(Year) %>%
      summarise(total_deaths = sum(Number_deaths, na.rm = TRUE))
    
    p <- ggplot(yearly_deaths, aes(x = Year, y = total_deaths)) +
      geom_line(color = "red", size = 1) +
      labs(title = "Yearly Cholera Deaths", y = "Total Deaths")
    
    ggplotly(p)
  })
  
  # Hotspot map (mock implementation)
  output$hotspot_map <- renderPlotly({
    plot_ly(type = "scattergeo", mode = "markers") %>%
      layout(title = "Cholera Hotspots Around the World")
  })
}


