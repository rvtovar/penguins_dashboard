library(tidyverse)
library(shiny)
library(bslib)
library(palmerpenguins)

data(penguins)

species <- penguins$species %>% unique()
islands <- penguins$island %>% unique()

# This is the UI
ui <- page_sidebar(
  title = "Palmer Penguins Dashboard",
  # Our Side bar
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
  # The tabsets
  tabsetPanel(
    tabPanel(
      title = "Distribution Plots",
      plotOutput(outputId = "bill_length_plot"),
      plotOutput(outputId = "bill_depth_plot")
    ),
    tabPanel(
      title = "Filtered Data",
      br(),
      downloadButton(outputId = "filtered_penguin_data_download", "Download Data"),
      dataTableOutput(output="filtered_penguin_data")
    ),
    tabPanel(
      title = "Scatter Plots",
      plotOutput(outputId = "length_v_depth_plot")
    )
  )
)


# All Server logic goes here
server <- function(input,output,session){
  filtered_penguins <- reactive({
    data <- penguins %>% filter(species %in% input$species)
    data <- data %>% filter(island %in% input$island)
    return(data)
  })
  
  output$filtered_penguin_data_download <- downloadHandler(
    filename = function(){
      paste("filtered_penguin_data",Sys.Date(),".csv",sep = "")
    },
    content = function(file){
      write.csv(filtered_penguins(),file)
    }
  )
  
  output$bill_length_plot <- renderPlot({
    filtered_penguins() %>% ggplot(aes(
      x = bill_length_mm,
      fill = species
    )) + geom_density(alpha = 0.6) + labs(
      title = "Bill Length Distribution",
      x = "Bill Length (mm)",
      y = "Count"
    ) + theme_minimal()
  })
  
  output$bill_depth_plot <- renderPlot({
    filtered_penguins() %>% ggplot(aes(
      x = bill_depth_mm,
      fill = species
    )) + geom_density(alpha = 0.6) + labs(
      title = "Bill Depth Distribution",
      x = "Bill Depth (mm)",
      y = "Count"
    ) + theme_minimal()
  })
  
  output$length_v_depth_plot <- renderPlot({
    filtered_penguins() %>% ggplot(aes(
      x = bill_length_mm,
      y = body_mass_g,
      color=species
    )) + geom_point()
  })
  
  output$filtered_penguin_data <- renderDataTable({
    filtered_penguins()
  })
}


#this runs the app
shinyApp(ui, server)