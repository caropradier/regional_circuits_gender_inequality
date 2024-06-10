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
  fig1_plot_server("fig1")
  fig2_plot_server("fig2")
  fig3_plot_server("fig3")
  fig4_plot_server("fig4")
}


##### RUN #####

shinyApp(ui, server)