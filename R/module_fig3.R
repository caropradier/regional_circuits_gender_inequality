fig3_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    plot <- function(dis,kw) {
      
      d <- results_list$plot_3_table %>% 
        mutate(tag = ifelse(my_label == "", paste0(label), my_label)) %>% 
        filter(level1 %in% dis) 
      
      if(kw == ""){
        d
      } else {
        d <- d %>% 
          filter(grepl(kw, representation))
      }
       
      
      p <- ggplot(data = d,aes(x=women,y=latam,size=count,color = fct_reorder(level1,order), 
                               text = paste0('</br><b>', tag,'</b>',
                                             '</br><i>',level1,'</i>',
                                             '</br>Representative terms: ',representation,
                                             '</br>Women authorship: ',round(women*100,2),'%',
                                             '</br>Published in Latin American journals or conferences: ',round(latam*100,2),'%')
                               ))+
        geom_point(alpha=0.5)+
        geom_hline(yintercept = m_l)+
        geom_vline(xintercept = m_w)+
        scale_color_manual(values=mycolors)+
        scale_size(range = c(2, 10))+
        theme_minimal()+
        theme(text = element_text(size = 12))+
        scale_x_continuous(labels = function(x) paste0(x*100,"%"))+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        labs(y="Published in Latin American journals or conferences",
             x="Women authorship",
             color="Discipline")+
        guides(size = "none")+
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    output$plot <- renderPlotly({
      plot(input$input_discipline, input$keyword)
    })
  })
}

fig3_plot_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Figure 3",
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("input_discipline"),
                    label = "Select disciplines",
                    choices = unique(results_list$plot_3_table$level1),
                    selected = unique(results_list$plot_3_table$level1),
                    multiple = TRUE
        ),
        textInput(ns("keyword"), "Search keyword:"),
        , width = 3),
      mainPanel(h2("The Latin American topic space"),
        plotlyOutput(ns("plot"), height = 700)%>% withSpinner(type = 5, color ="black")
                )
    )
  )
}