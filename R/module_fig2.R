fig2_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    f_plot_2a <- function() {
      d <- results_list$plot_2a_table
      
      p <- ggplot(data =d,aes(x = fct_reorder(level1,order), y = prop, fill = latam_journal,
                              text = paste0(round(prop*100,2))))+
        geom_col(position = "stack")+
        theme_minimal()+
        theme(text = element_text(size = 12))+
        scale_fill_brewer(palette = "Dark2",direction = 1)+
        theme(legend.position = "top")+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        labs(fill = "", x = "", y = "Publications in each circuit",
             title = "A. Proportion of documents according to the dissemination circuit and discipline")+
        coord_flip()  +
        theme(plot.title = element_text( margin = margin(0,0,-10,0)))+
        scale_x_discrete(labels=function(x) str_wrap(x,10))
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    f_plot_2b <- function() {
      d <- results_list$plot_2b_table
      
      p <- ggplot(data =d,aes(x = fct_reorder(level1,order), y = ratio, 
                              text = paste0(round(ratio,2))))+
        geom_col(fill = "#7570B3")+
        theme_minimal()+
        theme(text = element_text(size = 12))+
        scale_fill_brewer(palette = "Paired",direction = 1)+
        theme(legend.position = "bottom")+
        #scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        labs(fill = "", x = "", y = "Men to women authors ratio",
             title = "B. Ratio of men to women Latin-American authors, by discipline")+
        coord_flip() +
        theme(plot.title = element_text( margin = margin(0,0,-10,0)))+
        scale_x_discrete(labels=function(x) str_wrap(x,10))
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    f_plot_2c <- function() {
      d <- results_list$plot_2c_table
      
      p <- ggplot(data=d,aes(x=gender,y= `Real/Expected`,fill =gender,
                             text = paste0(round(`Real/Expected`,2))))+
        geom_col(position = "dodge")+
        theme_minimal()+
        theme(legend.position = "none")+
        theme(text = element_text(size = 12),
              axis.title.x = element_text(size = 11))+
        scale_fill_brewer(palette = "Paired",direction = 1)+
        labs(y = "Ratio between observed and expected \n % of publications in Latin America",
             x = "",
             title = "C. Ratio between observed and expected proportion \n of documents published in Latin-America")+
        geom_hline(yintercept = 0,size=.5)+
        coord_flip()
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    output$plot_2a <- renderPlotly({
      f_plot_2a()
    })
    output$plot_2b <- renderPlotly({
      f_plot_2b()
    })
    output$plot_2c <- renderPlotly({
      f_plot_2c()
    })
  })
}

fig2_plot_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Disciplines",
    sidebarLayout(
      sidebarPanel(
         width = 0
      ),
      mainPanel(h2("Disciplines and dissemination circuits by gender"),
                h4("1993-2022"),
                br(),
                br(),
        plotlyOutput(ns("plot_2a"), height = 400)%>% withSpinner(type = 5, color ="black"),
        br(),
        plotlyOutput(ns("plot_2b"), height = 400)%>% withSpinner(type = 5, color ="black"),
        br(),
        plotlyOutput(ns("plot_2c"), height = 400)%>% withSpinner(type = 5, color ="black")
        )
    )
  )
}