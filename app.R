# Shiny App using the cancer_sample dataset from the cancaer package
# Features:
# 1. Dynamic plot (diagnosis counts by radius_mean range)
# 2. Interactive table showing diagnosis counts
# 3. Allows user to filter by radius_mean and diagnosis (B/M)
# 4. Download plot as PNG
# 5. Download table as CSV

library(shiny)
library(DT)
library(dplyr)
library(datateachr)

# Load cancer_sample dataset
data("cancer_sample")

# Define UI
ui <- fluidPage(
  titlePanel("Cancer Sample: Radius Mean Filter App"),
  
  # Brief description and instructions for the user
  p("This Shiny App allows you to explore the cancer_sample dataset from the cancaer package. ",
    "You can filter the data by radius_mean range and diagnosis type (B or M). ",
    "The app dynamically updates a plot showing the counts of B and M diagnoses, ",
    "an interactive table showing diagnosis counts, and allows you to download both the plot and the table."),
  
  sidebarLayout(
    sidebarPanel(
      # Slider to filter radius_mean
      sliderInput("radiusRange",
                  "Select radius_mean range:",
                  min = min(cancer_sample$radius_mean, na.rm = TRUE),
                  max = max(cancer_sample$radius_mean, na.rm = TRUE),
                  value = c(min(cancer_sample$radius_mean, na.rm = TRUE),
                            max(cancer_sample$radius_mean, na.rm = TRUE))),
      
      # Checkbox to filter diagnosis type
      checkboxGroupInput("diagnosisFilter",
                         "Select diagnosis:",
                         choices = c("B", "M"),
                         selected = c("B", "M")),
      
      # Download buttons
      downloadButton("downloadPlot", "Download Plot (PNG)"),
      downloadButton("downloadTable", "Download Table (CSV)")
    ),
    
    mainPanel(
      # Display number of filtered results
      textOutput("nResults"),
      
      # Plot output
      plotOutput("histPlot"),
      
      # Table output
      DT::dataTableOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive dataset based on radius_mean range and diagnosis filter
  filtered_data <- reactive({
    cancer_sample %>%
      filter(radius_mean >= input$radiusRange[1],
             radius_mean <= input$radiusRange[2],
             diagnosis %in% input$diagnosisFilter)
  })
  
  # Display number of filtered records
  output$nResults <- renderText({
    n <- nrow(filtered_data())
    paste("We found", n, "records within the selected range and diagnosis filter.")
  })
  
  # Plot diagnosis counts
  output$histPlot <- renderPlot({
    data_to_plot <- filtered_data()
    if(nrow(data_to_plot) == 0){
      plot(1, type="n", xlab="", ylab="", main="No data in this range and diagnosis filter")
      return()
    }
    counts <- table(data_to_plot$diagnosis)
    barplot(counts,
            col = "steelblue",
            border = "white",
            ylab = "Count",
            main = paste("Diagnosis Counts (Radius Mean:",
                         round(input$radiusRange[1],1), "-",
                         round(input$radiusRange[2],1), ")"))
  })
  
  # Display table of diagnosis counts
  output$table <- DT::renderDataTable({
    filtered_data() %>%
      group_by(diagnosis) %>%
      summarise(count = n(), .groups = "drop")
  }, options = list(pageLength = 10, searching = FALSE))  # Disable search box
  
  # Download plot as PNG
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("diagnosis_plot.png") },
    content = function(file) {
      png(file)
      data_to_plot <- filtered_data()
      if(nrow(data_to_plot) > 0){
        counts <- table(data_to_plot$diagnosis)
        barplot(counts,
                col = "steelblue",
                border = "white",
                ylab = "Count",
                main = paste("Diagnosis Counts (Radius Mean:",
                             round(input$radiusRange[1],1), "-",
                             round(input$radiusRange[2],1), ")"))
      }
      dev.off()
    }
  )
  
  # Download table as CSV
  output$downloadTable <- downloadHandler(
    filename = function() { paste("diagnosis_table.csv") },
    content = function(file) {
      data_to_save <- filtered_data() %>%
        group_by(diagnosis) %>%
        summarise(count = n(), .groups = "drop")
      write.csv(data_to_save, file, row.names = FALSE)
    }
  )
}

# Run the Shiny App
shinyApp(ui = ui, server = server)