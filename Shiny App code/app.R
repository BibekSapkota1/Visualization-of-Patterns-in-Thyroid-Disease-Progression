library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(RColorBrewer)
library(shinyjs)
library(DT)  
library(fontawesome)

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
            body {
                background-color: #f4f6f7;
                color: #2c3e50;
                font-family: 'Roboto', sans-serif;
                margin-bottom: 50px;
            }
            .header {
                background: linear-gradient(90deg, #2980b9, #6dd5fa, #ffffff);
                color: white;
                padding: 20px;
                text-align: center;
                border-radius: 8px;
                box-shadow: 0 4px 8px rgba(0,0,0,0.1);
            }
            .header h2 {
                margin: 0;
                font-weight: 700;
                font-size: 28px;
                letter-spacing: 1px;
            }
            .footer {
                background-color: #2C3E50;
                color: white;
                text-align: center;
                position: fixed;
                bottom: 0;
                width: 100%;
                height: 35px;
                line-height: 35px;
                font-size: 13px;
                box-shadow: 0 -2px 5px rgba(0, 0, 0, 0.2);
            }
            .panel {
                background-color: white;
                border: 1px solid #ddd;
                border-radius: 8px;
                padding: 20px;
                box-shadow: 0 4px 8px rgba(0,0,0,0.05);
                margin-bottom: 20px;
                margin-top: 10px;
            }
            .btn-primary {
                background-color: #3498db;
                color: white;
                border-radius: 20px;
                border: none;
                padding: 10px 20px;
                transition: background-color 0.3s ease;
                box-shadow: 0 3px 6px rgba(0, 0, 0, 0.1);
            }
            .btn-primary:hover {
                background-color: #2980b9;
            }
            .form-control {
                border-radius: 20px;
            }
            .slider-input {
                border-radius: 20px;
            }
            .dataTable-container {
                overflow-x: auto;
                padding: 10px;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
            }
            .plotly-container {
                top: 20px;
            }
            .centered-header {
                text-align: center;
                margin-top: 20px;
                font-weight: bold;
            }
            .instruction-list {
                position: fixed;
                top: 70px;
                right: 10px;
                background-color: #ecf0f1;
                border-left: 5px solid #3498db;
                padding: 10px;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
                transition: transform 0.3s ease, box-shadow 0.3s ease;
                z-index: 999;
                display: none;
                width: 300px;
                border-top: 2px solid black;
                border-right: 3px solid black;
                border-bottom: 2px solid black;
            }
            .instruction-list:hover {
                transform: scale(1.05);
                box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
            }
            .show-instructions {
                cursor: pointer;
                position: fixed;
                top: 23px;
                right: 20px;
                z-index: 1000;
                border-radius: 20px;
                box-shadow: 0 3px 6px rgba(0,0,0,0.1);
            }
        "))
  ),
  
  div(class = "header", h2("Visualization of Thyroid Function Data")),
  
  tabsetPanel(
    tabPanel(
      title = tagList(icon("table", class = "fa-lg"), " Dataset"),
      fluidRow(
        column(12, div(class = "panel",
                       h3(icon("database"), " Dataset"),
                       p("Explore the thyroid function dataset:"),
                       div(class = "dataTable-container",  
                           DT::DTOutput("dataTable")
                       )
        ))
      )
    ),
    tabPanel(
      title = tagList(icon("chart-bar", class = "fa-lg"), " Summary"),
      fluidRow(
        column(12, div(class = "panel",
                       h3(icon("clipboard-list"), " Dataset Summary"),
                       p("Summary of key statistics:"),
                       verbatimTextOutput("columnSummary")
        ))
      )
    ),
    tabPanel(
      title = tagList(icon("chart-line", class = "fa-lg"), " Visualization"),
      fluidRow(
        column(3, div(class = "visual",
                      h4(icon("sliders-h"), " Visualization Controls"),
                      selectInput("graphType", "Select Graph Type:",
                                  choices = c("Histogram" = "histogram",
                                              "Bar Plot" = "bar",
                                              "Density Plot" = "density",
                                              "Scatter Plot" = "scatter")),
                      
                      conditionalPanel(
                        condition = "input.graphType !== 'bar'",
                        sliderInput("ageRange", "Select Age Range:", 
                                    min = 20, max = 80, value = c(20, 80))
                      ),
                      conditionalPanel(
                        condition = "input.graphType === 'histogram'",
                        sliderInput("bins", "Number of Bins:", 
                                    min = 5, max = 50, value = 30)
                      ),
                      conditionalPanel(
                        condition = "input.graphType === 'scatter'",
                        selectInput("yVarScatter", "Select Y Variable:", 
                                    choices = names(data))
                      ),
                      conditionalPanel(
                        condition = "input.graphType === 'bar'",
                        selectInput("xVarBar", "Select X Variable:", choices = names(data)),
                        selectInput("fillVarBar", "Select Fill Variable:", choices = names(data))
                      ),
                      conditionalPanel(
                        condition = "input.graphType === 'density'",
                        selectInput("fillVarDensity", "Select Fill Variable for Density Plot:", choices = names(data))
                      ),
                      conditionalPanel(
                        condition = "input.graphType === 'bar' || input.graphType === 'density'",
                        checkboxInput("showLegend", "Show Legend", TRUE)
                      )
        )),
        column(9, div(class = "panel",
                      div(class = "plotly-container",
                          plotlyOutput("plot")
                      ),
                      h5("Visualization Output", class = "centered-header")
        ))
      )
    )
  ),
  
  fluidRow(
    column(12, div(class = "footer",
                   h5("Developed by Bibek Sapkota (Student_ID: 23189618)")
    ))
  ),
  
  actionButton("show_instructions", "Show Instructions", class = "show-instructions"),
  
  tags$ul(id = "instruction_list", class = "instruction-list",
          tags$li("Dataset Tab: View the complete thyroid function dataset. You can scroll horizontally to see all columns."),
          tags$li("Summary Tab: Get a summary of key statistics for each column in the dataset."),
          tags$li("Visualization Tab: Select different types of visualizations, adjust parameters, and see interactive plots."),
          tags$li("Use the sliders and dropdown menus to filter data and customize the plots.")
  )
)

# Server Function
server <- function(input, output, session) {
  data <- read.csv('Thyroid_Diff.csv')
  
  output$dataTable <- DT::renderDT({
    data
  }, options = list(pageLength = 25))
  
  output$columnSummary <- renderPrint({
    summary_list <- lapply(names(data), function(column) {
      if (is.numeric(data[[column]])) {
        mean_val <- mean(data[[column]], na.rm = TRUE)
        median_val <- median(data[[column]], na.rm = TRUE)
        std_dev <- sd(data[[column]], na.rm = TRUE)
        min_val <- min(data[[column]], na.rm = TRUE)
        max_val <- max(data[[column]], na.rm = TRUE)
        
        paste(
          column, " (Numeric)",
          "\n Mean:", round(mean_val, 2),
          "\n Median:", round(median_val, 2),
          "\n Standard Deviation:", round(std_dev, 2),
          "\n Min:", min_val,
          "\n Max:", max_val,
          "\n"
        )
        
      } else if (is.factor(data[[column]]) || is.character(data[[column]])) {
        mode_val <- names(sort(table(data[[column]]), decreasing = TRUE)[1])
        mode_count <- max(table(data[[column]]))
        unique_vals <- unique(data[[column]])
        
        paste(
          column, " (Categorical)",
          "\n Mode:", mode_val,
          "\n Mode Count:", mode_count,
          "\n Unique Values:", paste(unique_vals, collapse = ", "),
          "\n"
        )
      }
    })
    
    cat(unlist(summary_list), sep = "\n")
  })
  
  observe({
    updateSelectInput(session, "yVarScatter", choices = names(data), selected = "Age")
    updateSelectInput(session, "xVarBar", choices = names(data), selected = "Gender")
    updateSelectInput(session, "fillVarBar", choices = names(data), selected = "Smoking")
    updateSelectInput(session, "fillVarDensity", choices = names(data), selected = "Smoking")
  })
  
  output$plot <- renderPlotly({
    req(input$graphType)
    
    plot_input <- switch(input$graphType,
                         histogram = ggplot(filtered_data(), aes(x = Age)) +
                           geom_histogram(bins = input$bins, fill = "orange", color = "black") +
                           labs(title = "Histogram of Age", x = "Age", y = "Frequency") +
                           theme_minimal() +
                           theme(legend.position = ifelse(input$showLegend, "right", "none")),
                         
                         bar = ggplot(data, aes_string(x = input$xVarBar, fill = input$fillVarBar)) +
                           geom_bar(position = "dodge", show.legend = input$showLegend) +
                           labs(title = paste("Bar Plot of", input$xVarBar, "and", input$fillVarBar), x = input$xVarBar, y = "Count") +
                           theme_minimal() +
                           theme(legend.position = ifelse(input$showLegend, "right", "none")),
                         
                         density = ggplot(filtered_data(), aes(x = Age, fill = .data[[input$fillVarDensity]])) +
                           geom_density(alpha = 0.5, show.legend = input$showLegend) +
                           labs(title = paste("Density Plot of Age by", input$fillVarDensity), x = "Age", y = "Density") +
                           theme_minimal() +
                           theme(legend.position = ifelse(input$showLegend, "right", "none")),
                         
                         scatter = ggplot(filtered_data(), aes(x = Age, y = .data[[input$yVarScatter]], color = .data[[input$yVarScatter]])) +
                           geom_point() +
                           labs(title = paste("Scatter Plot of Age vs", input$yVarScatter), x = "Age", y = input$yVarScatter) +
                           theme_minimal() +
                           theme(legend.position = ifelse(input$showLegend, "right", "none"))
    )
    
    ggplotly(plot_input)
  })
  
  filtered_data <- reactive({
    if (input$graphType %in% c("histogram", "density", "scatter")) {
      data %>% filter(Age >= input$ageRange[1], Age <= input$ageRange[2])
    } else {
      data
    }
  })
  observeEvent(input$show_instructions, {
    toggle("instruction_list")
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
