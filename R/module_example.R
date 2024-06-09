sample_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    plot <- function(input_cut) {
      d <- diamonds[sample(nrow(diamonds), 1000), ] %>%
        filter(cut == input_cut)
      
      p <- ggplot(data = d, aes(x = carat, y = price)) +
        geom_point(aes(text = paste("Clarity:", clarity))) +
        geom_smooth(aes(colour = cut, fill = cut)) +
        scale_color_manual(values = paleta_1) +
        scale_fill_manual(values = paleta_1)
      
      
      ggplotly(p)
    }
    
    output$plot <- renderPlotly({
      plot(input$input_cut)
    })
  })
}

sample_plot_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "sample 1",
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("input_cut"),
                    label = "select cut",
                    choices = diamonds$cut %>% unique(),
                    selected = "Premium",
                    multiple = FALSE
        )
      ),
      mainPanel(plotlyOutput(ns("plot")))
    )
  )
}