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
    fig1_plot_ui("fig1"),
    fig2_plot_ui("fig2"),
    fig3_plot_ui("fig3"),
    fig4_plot_ui("fig4"),
    methods
    
  )
}