library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(gt)
library(markdown)

# load('data.Rdata')

about_ui <- tabPanel(
  title = "About",
  includeMarkdown("README.md")
)

methods <- tabPanel(
  title = "Methods",
  includeMarkdown("Methods.md")
)


main_ui <- {
  navbarPage(
    "Science for whom?",
    about_ui,
    sample_plot_ui("ejemplo"),
    methods
    
  )
}