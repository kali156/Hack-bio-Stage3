
# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(plotly)
library(bslib)
library(shinycssloaders)
# Load data
data <- read.csv("cholera joined data.csv")

# custom css design 
custom_css <-"
  body {
    font-family: 'Roboto', sans-serif;
    background: linear-gradient(135deg, #2c3e50 0%, #4ca1af 100%);
                                color: #ecf0f1;
  }
.main-header .logo {
  font-family: 'Roboto', sans-serif;
  font-weight: bold;
  background-color: #34495e !important;
    color: #ecf0f1 !important;
}
.content-wrapper, .main-footer {
  background-color: #2c3e50;
}
.box {
  background-color: #34495e;
    border: none;
  box-shadow: 0px 4px 8px rgba(0,0,0,0.1);
  border-radius: 10px;
}
.value-box {
  background-color: #2980b9 !important;
    color: #ffffff !important;
    border-radius: 10px;
  box-shadow: 0px 4px 8px rgba(0,0,0,0.1);
}
.value-box .icon {
  font-size: 40px;
}
.value-box h3, .value-box p {
  color: #ffffff;
}
.navbar-custom-menu .nav > li > a {
  color: #ecf0f1 !important;
}
.form-control, .selectize-input, .slider-input {
  background-color: #34495e;
    color: #ffffff;
    border-radius: 5px;
  border: 1px solid #2c3e50; 
}
.slider .slider-handle {
  background-color: #2980b9 !important;
}
h1, h2, h3, h4, h5 {
  font-weight: 300;
  color: #ecf0f1;
}"
# UI is defined here
 dashboardPage(
  skin = "black",
  dashboardHeader(title = "Cholera Data Dive"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("bar-chart")),
      menuItem("Country Trends", tabName = "country", icon = icon("globe")),
      menuItem("Yearly Analysis", tabName = "yearly", icon = icon("line-chart")),
      menuItem("Hotspot Map", tabName = "hotspot", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap"),
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_cases"),
                valueBoxOutput("total_deaths"),
                valueBoxOutput("avg_fatality")
              ),
              fluidRow(
                box(width = 12, plotlyOutput("overall_trend_plot"))
              )
      ),
      
      # Country-specific analysis tab
      tabItem(tabName = "country",
              fluidRow(
                box(selectInput("country", "Select Country:", 
                                choices = unique(data$Country)), width = 4),
                box(sliderInput("yearRange", "Select Year Range:", 
                                min = min(data$Year), 
                                max = max(data$Year), 
                                value = c(min(data$Year), max(data$Year)),
                                sep = ""), width = 8)
              ),
              fluidRow(
                box(width = 12, plotlyOutput("country_plot"))
              )
      ),
      
      # Yearly analysis tab
      tabItem(tabName = "yearly",
              fluidRow(
                box(plotlyOutput("yearly_cases_plot"), width = 6),
                box(plotlyOutput("yearly_deaths_plot"), width = 6)
              )
      ),
      
      # Cholera Hotspot map tab
      tabItem(tabName = "hotspot",
              fluidRow(
                box(width = 12, plotlyOutput("hotspot_map"))
              )
      )
    )
  )
)


