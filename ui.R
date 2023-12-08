# Drake A. Eserhaut
# DATA 824 - Data Visualization
# Final Project - Build a Shiny Application
# December 7th, 2023

setwd("/Users/eserhaut/Desktop/DATA824_DataViz/Assignments/ShinyApp/")

library(readxl)
DATA <- read_excel("IPFdata.USE.xlsx")

###########################################
# ui.R Script
###########################################
library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Powerlifting Competition Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("athlete_search", "Search Athlete:",
                     choices = unique(DATA$Name),
                     multiple = FALSE,
                     options = list('create' = TRUE, 'persist' = FALSE)),
      uiOutput("athleteInfo"),
      uiOutput("regressionSlopeText"),
      br(),
      uiOutput("example_names"),
      br(),
      plotOutput("SBDpie"),
      br(),
      selectInput("y_axis_variable", "Choose Performance Variable:",
                  choices = c("TotalKg", "Squat", "Bench", "Deadlift"),
                  selected = "TotalKg"),
      selectInput("sex_filter", "Choose Gender:",
                  choices = c("M", "F"),
                  selected = "M")
    ),
    mainPanel(
      plotOutput("totalPlot"),
      plotOutput("SBDPlot"),
      plotOutput("bodyweightPlot"),
      htmlOutput("rSquaredText"),
      plotOutput("regressionLinePlot")
    )
  )
))