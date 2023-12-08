# Drake A. Eserhaut
# DATA 824 - Data Visualization
# Final Project - Build a Shiny Application
# December 7th, 2023

###########################################
# server.R Script
###########################################
library(tidyverse)
library(Hmisc)
library(egg) # used for tagging plot facets
library(ggpubr)
library(ggplot2)
library(scales)
library(irr)

### Shiny and other packages
library(shiny)
library(shinydashboard)
library(DT)
library(bslib)
library(modeldata)
library(DataExplorer)
library(plotly)

setwd("/Users/eserhaut/Desktop/DATA824_DataViz/Assignments/ShinyApp/")

library(readxl)
DATA <- read_excel("IPFdata.USE.xlsx")

# server.R
library(shiny)
library(ggplot2)
library(cowplot)

shinyServer(function(input, output, session) {
  
  # Pre-select a specific athlete when the app starts
  selected_athlete <- "Kristen Dunsmore"  # Change this to the desired athlete's name
  
  # Update the selectize Input to pre-select the desired athlete
  updateSelectizeInput(session, "athlete_search", selected = selected_athlete)
  
  # Render example names
  output$example_names <- renderUI({
    HTML("<b>Try searching for:</b><br>• Ray Williams<br>• Ashton Rouska<br>• Bryce Krawczyk<br>• Bryce Lewis<br>• Taylor Atwood<br>• Kristen Dunsmore")
  })
  
  # Filter data based on user input
  filtered_data <- reactive({
    DATA %>%
      filter(str_detect(Name, input$athlete_search))
  })
  
  # Render pie chart
  output$SBDpie <- renderPlot({
    averages <- filtered_data() %>%
      group_by() %>%
      summarise(
        Squat = mean(Squat, na.rm = TRUE),
        Bench = mean(Bench, na.rm = TRUE),
        Deadlift = mean(Deadlift, na.rm = TRUE)
      )
    
    labels <- c("Squat", "Bench", "Deadlift")
    values <- c(averages$Squat, averages$Bench, averages$Deadlift)
    
    pie_data <- data.frame(labels, values)
    
    # Calculate percentages and round up to the nearest whole number
    pie_data$percentage <- scales::percent(pie_data$values / sum(pie_data$values), accuracy = 1)
    
    # Organize the levels in the legend
    pie_data$labels <- factor(pie_data$labels, levels = c("Squat", "Bench", "Deadlift"))
    
    pie_chart <- ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      theme_void() +
      geom_text(aes(label = percentage), 
                position = position_stack(vjust = 0.5),
                size = 8,
                fontface = "bold",
                color = "white") +
      scale_fill_manual(values = c("Squat" = "dodgerblue2", "Bench" = "red3", "Deadlift" = "green4")) +
      ggtitle("\nPowerlifting Total - Composition\n") +
      labs(fill = "") +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.position = "top",
            legend.box.margin = margin(0, 0, 0, 0),
            legend.text = element_text(size = 12)
      )
    print(pie_chart)
  })
  
  # Render dynamic text element for regression slope
  output$regressionSlopeText <- renderUI({
    athlete_data <- filtered_data()
    
    # Fit a linear regression model
    regression_model <- lm(TotalKg ~ Year, data = athlete_data)
    
    # Extract the slope coefficient
    slope <- coefficients(regression_model)[2]
    
    color <- ifelse(slope >= 0, "green", "red")
    
    HTML(paste("<span style='font-size: 14px; font-weight: bold; color:", color, "'>Powerlifting Total - Projected Annual Change:", round(slope, 2), "%</span>"))
  })
  
  # Set the style for the dynamic text
  outputOptions(output, "regressionSlopeText", renderText(
    inlineCSS(list("font-size" = "14px", "font-weight" = "bold"))
  ))
  
  # Render dynamic text elements for Age, Division, and Dots
  output$athleteInfo <- renderUI({
    athlete_data <- filtered_data()
    most_recent_year <- athlete_data[which.max(athlete_data$Year), ]
    
    age_text <- paste("Age: ", most_recent_year$Age)
    division_text <- paste("Division: ", most_recent_year$Division)
    dots_text <- paste("Dots: ", most_recent_year$Dots)
    
    HTML(paste("<span style='font-size: 12px;'>", 
               "<span style='font-weight: bold;'>", age_text, "</span><br>",
               "<span style='font-weight: bold;'>", division_text, "</span><br>",
               "<span style='font-weight: bold;'>", dots_text, "</span>",
               "</span>"))
  })
  
  # Render plot
  output$totalPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Year)) +
      geom_line(aes(y = TotalKg, color = "black"), size = 1.5) +
      geom_label(aes(y = TotalKg, label = TotalKg), hjust = 0.5, color = "black", size = 3, fill = "white", fontface = "bold", label.padding = unit(0.25, "lines")) +
      labs(title = "Powerlifting Total Over Time",
           x = "\nYear",
           y = "Kilograms, kg",
           color = "") +
      scale_x_continuous(breaks = seq(2015, 2023, by = 1), minor_breaks = seq(2015, 2023, by = 1)) +
      scale_color_manual(values = c("black" = "black"), labels = c("Total")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.text.y = element_text(size = 10, face = "bold"),
            axis.text.x = element_text(size = 10, face = "bold"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_line(size = 0.75, color = "black"),
            plot.margin = unit(c(1, 0.25, 1, 0.25), "cm"),
            legend.position = "none"
      )
  })
  
  # Render plot
  # R color names --> https://www.datanovia.com/en/blog/awesome-list-of-657-r-color-names/
  output$SBDPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Year)) +
      geom_line(aes(y = Squat, color = "Squat"), size = 1.5) +
      geom_label(aes(y = Squat, label = Squat), hjust = 0.5, color = "black", size = 3, fill = "white", fontface = "bold", label.padding = unit(0.25, "lines")) +  # Add rectangular label shape
      geom_line(aes(y = Bench, color = "Bench"), size = 1.5) +
      geom_label(aes(y = Bench, label = Bench), hjust = 0.5, color = "black", size = 3, fill = "white", fontface = "bold", label.padding = unit(0.25, "lines")) +  # Add rectangular label shape
      geom_line(aes(y = Deadlift, color = "Deadlift"), size = 1.5) +
      geom_label(aes(y = Deadlift, label = Deadlift), hjust = 0.5, color = "black", size = 3, fill = "white", fontface = "bold", label.padding = unit(0.25, "lines")) +  # Add rectangular label shape
      labs(title = "SBD Performances Over Time",
           x = "\nYear",
           y = "Kilograms, kg",
           color = "") +
      scale_x_continuous(breaks = seq(2015, 2023, by = 1), minor_breaks = seq(2015, 2023, by = 1)) +
      scale_color_manual(
        values = c("Squat" = "dodgerblue2", "Bench" = "red3", "Deadlift" = "green4"),
        breaks = c("Squat", "Bench", "Deadlift")
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.text.y = element_text(size = 10, face = "bold"),
            axis.text.x = element_text(size = 10, face = "bold"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_line(size = 0.75, color = "black"),
            plot.margin = unit(c(0.25, 0.25, 1, 0.25), "cm"),
            legend.position = "top",
            legend.box.margin = margin(0, 0, -1, 0),
            legend.text = element_text(size = 12))
  })
  
  # Render plot
  output$bodyweightPlot <- renderPlot({
    # Calculate the average bodyweight
    avg_bodyweight <- mean(filtered_data()$BodyweightKg, na.rm = TRUE)
    
    ggplot(filtered_data(), aes(x = Year)) +
      geom_line(aes(y = BodyweightKg, color = "black"), linetype = "dashed", size = 1, alpha = 0.8) +
      geom_point(aes(y = BodyweightKg, color = "black"), size = 3) +
      geom_label(aes(y = BodyweightKg, label = BodyweightKg), hjust = 0.5, color = "black", size = 3, fill = "white", fontface = "bold", label.padding = unit(0.25, "lines")) +
      labs(title = "Bodyweight Trends Over Time",
           x = "\nYear",
           y = "Kilograms, kg",
           color = "") +
      scale_x_continuous(breaks = seq(2015, 2023, by = 1), minor_breaks = seq(2015, 2023, by = 1)) +
      scale_color_manual(values = c("black" = "black"), labels = c("Bodyweight")) +
      scale_y_continuous(limits = c(avg_bodyweight - 15, avg_bodyweight + 15)) + # y-axis limits set at +/- 15kg of the athlete's avg. bw
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.text.y = element_text(size = 10, face = "bold"),
            axis.text.x = element_text(size = 10, face = "bold"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_line(size = 0.75, color = "black"),
            plot.margin = unit(c(0.25, 0.25, 1, 0.25), "cm"),
            legend.position = "none"
      )
  })
  
  
  # Reactive expression to filter data by selected sex
  DATA.filtered <- reactive({
    DATA %>%
      filter(Sex == input$sex_filter)
  })
  
  # Render the regression line plot
  output$regressionLinePlot <- renderPlot({
    dynamicAVGplot <- ggplot(DATA.filtered(), aes(x = BodyweightKg)) +
      geom_point(aes_string(y = input$y_axis_variable, color = input$y_axis_variable), size = 3) +
      geom_smooth(aes_string(y = input$y_axis_variable), method = "lm", se = FALSE, linetype = "dashed", color = "black") +
      labs( x = "\nBodyweight, kg",
            y = "Kilograms, kg",
            color = input$y_axis_variable) +
      scale_x_continuous(breaks = seq(0, 200, by = 25), minor_breaks = seq(0, 200, by = 12.5)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.text.y = element_text(size = 10, face = "bold"),
            axis.text.x = element_text(size = 10, face = "bold"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.x = element_line(size = 0.75, color = "black"),
            axis.ticks.y = element_line(size = 0.75, color = "black"),
            plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
            legend.position = "right",
            legend.box.margin = margin(0, 0, 0, 0)
      )
    
    # Check if the variable is continuous or discrete
    if (is.numeric(DATA.filtered()[[input$y_axis_variable]])) {
      dynamicAVGplot <- dynamicAVGplot + scale_color_continuous(input$y_axis_variable)
    } else {
      dynamicAVGplot <- dynamicAVGplot + scale_color_manual(values = c("TotalKg" = "black", "Squat" = "dodgerblue2", "Bench" = "red3", "Deadlift" = "green4"))
    }
    
    print(dynamicAVGplot)
  })
  
  # Render text for R-squared and slope
  output$rSquaredText <- renderUI({
    # Fit a linear regression model
    lm_eq <- lm(DATA.filtered()[[input$y_axis_variable]] ~ DATA.filtered()$BodyweightKg)
    r_squared_value <- round(summary(lm_eq)$r.squared, 2)
    slope <- round(coef(lm_eq)[2], 2)
    
    # HTML styling for centering, font size, and boldness
    HTML(paste("<div style='text-align: center; font-size: 16px; font-weight: bold;'>",
               paste(input$y_axis_variable, "vs Bodyweight - R-squared =", r_squared_value, ", Slope =", slope),
               "</div>"))
  })
  
})