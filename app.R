library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)
library(DT)
library(plotly)
library(gridExtra)
library(shinyjs)



ui <- fluidPage(
  theme = shinytheme("paper"),
  
  # shinyjs::useShinyjs(),
  # introjsUI(),
  
  uiOutput(outputId = "main_ui")
)


##### server #####

server <- function(input, output, session) {
  # render UI
  output$main_ui <- renderUI({
    main_ui
  })
  
  ##########
  
  # Output modules ----------------------------------------------------------
  sample_plot_server("ejemplo")
}


##### RUN #####

shinyApp(ui, server)