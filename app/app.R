library(shiny)
library(tidyverse)
library(bslib)
library(palmerpenguins)

data(penguins)

species <- penguins$species %>% unique()
islands <- penguins$island %>% unique()

ui <- page_sidebar(
  title = "Palmer Penguins",
  sidebar = sidebar(
    checkboxGroupInput(
      inputId = "species",
      label = "Penguin Species",
      choices = species,
      selected = species
    ),
    checkboxGroupInput(
      inputId = "island",
      label = "Penguin Island",
      choices = islands,
      selected = islands
    )
  ),
  tabsetPanel(
    tabPanel(
      title = "Plots",
      plotOutput(outputId = "bill_length_plot"),
      plotOutput(outputId = "bill_depth_plot")
    ),
    tabPanel(
      title = "Filtered Data",
      br(),
      downloadButton(outputId = "filtered_penguin_data_download", "Download Data"),
      tableOutput("filtered_penguin_data")
    )
  )
)

server <- function(input, output, session){
  filtered_penguins <- reactive({
    data <- penguins[penguins$species %in% input$species,]
    data <- data[data$island %in% input$island,]
    return(data)
  })
  
  output$bill_length_plot <- renderPlot({
    ggplot(
      filtered_penguins(),aes(
        x = bill_length_mm,
        fill = species
      )
    ) + geom_density(alpha = 0.6) + labs(
      title = "Bill Length Distubtion",
      x = "Bill Length (mm)",
      y = "Count"
    ) + theme_minimal()
  })
  
  output$bill_depth_plot <- renderPlot({
    ggplot(
      filtered_penguins(),
      aes(
        x = bill_depth_mm,
        fill = species
      )
    ) + geom_density(alpha = 0.6) + labs(
      title = "Bill Depth Distribution",
      x = "Bill Depth (mm)",
      y = "Count"
    ) + theme_minimal()
  })
  
  output$filtered_penguin_data_download <- downloadHandler(
    filename = function(){
      paste("filtered_penguins", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(filtered_penguins(), file, row.names = FALSE)
    }
  )
  
  output$filtered_penguin_data <- renderTable({
    filtered_penguins()
  })
}
shinyApp(ui, server)
