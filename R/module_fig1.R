fig1_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    f_plot_1a <- function() {
      d <- results_list$plot_1a_table
      
      p <- ggplot(data=d,aes(y=value,x=as.numeric(pub_year),group = ind,color = gender, linetype = Circuit,
                             text = paste0('</br>',gender, ' - ', Circuit,
                                           '</br>',round(value,0),
                                           '</br>Publication year: ',pub_year)
                             ))+
        geom_line()+
        theme_minimal()+
        theme(legend.position = "bottom")+
        theme(text = element_text(size =test_size_fig1))+
        scale_color_brewer(palette = "Paired",direction = 1)+
        labs(y = "Number of publications",
             x = "Publication year", fill = "", color = "", linetype = "",
             title= "A")+
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = "text")%>% 
        layout(font = list(family = "Arial"))
    }
    
    f_plot_1b <- function() {
      d <- results_list$plot_1b_table
      
      p <- ggplot(data =d,aes(y=value,x=as.numeric(pub_year),group = ind,color = ind, linetype = ind,
                              text = paste0('</br>',ind,
                                            '</br>',round(value*100,2),
                                            '</br>Publication year: ',pub_year)))+
        geom_line()+
        theme_minimal()+
        theme(legend.position = "bottom")+
        theme(text = element_text(size = test_size_fig1))+
        scale_color_viridis(discrete = TRUE,option = "F", begin = .2, end =.5)+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        labs(x = "Publication year", 
             y = "Women authors / \n Women authorship",
             color = "",  linetype = "",
             title = "B"
        )+
        guides(fill = guide_legend(reverse=TRUE))+
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    f_plot_1c <- function() {
      d <- results_list$plot_1c_table
      
      p <- ggplot(data=d,aes(y=prop,x=as.numeric(pub_year),color = ind,group=ind ,linetype = ind ,
                             text = paste0('</br>',ind,
                                           '</br>',round(prop*100,2),
                                           '</br>Publication year: ',pub_year)))+
        geom_line()+
        theme_minimal()+
        theme(legend.position = "bottom")+
        theme(text = element_text(size =test_size_fig1))+
        scale_color_manual(values = c("#A6CEE3", "black", "#1F78B4"))+
        scale_linetype_manual(values=c("dashed", "solid", "dashed")) +  
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        labs(y = "Published in Latin American \n journals or conferences",
             x = "Publication year", color = "",linetype ="",
             title= "C")+
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    # f_plot_1d <- function() {
    #   d <- results_list$plot_1d_table
    #   
    #   p <- ggplot(data =d,aes(x = fct_reorder(level1,order), y = prop, fill = latam_journal,
    #                           text = paste0(round(prop*100,2))))+
    #     geom_col(position = "stack")+
    #     theme_minimal()+
    #     theme(text = element_text(size = 12))+
    #     scale_fill_brewer(palette = "Dark2",direction = 1)+
    #     theme(legend.position = "top")+
    #     scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
    #     labs(fill = "", x = "", y = "Publications in each circuit",
    #          title = "A")+
    #     coord_flip()  +
    #     theme(plot.title = element_text( margin = margin(0,0,-10,0)))+
    #     scale_x_discrete(labels=function(x) str_wrap(x,10))
    #   
    #   
    #   ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    # }
    
    output$plot_1a <- renderPlotly({
      f_plot_1a()
    })
    output$plot_1b <- renderPlotly({
      f_plot_1b()
    })
    output$plot_1c <- renderPlotly({
      f_plot_1c()
    })
    # output$plot_1d <- renderPlotly({
    #   f_plot_1d()
    # })
    
    
  })
}

fig1_plot_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Figure 1",
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("input_cut"),
                    label = "select cut",
                    choices = diamonds$cut %>% unique(),
                    selected = "Premium",
                    multiple = FALSE
        )
      ),
      mainPanel(h2("Gendered research dissemination circuits in Latin America"),
                h4("1993-2022"),
                plotlyOutput(ns("plot_1a"), height = 400)%>% withSpinner(type = 5, color ="black"),
                plotlyOutput(ns("plot_1b"), height = 400)%>% withSpinner(type = 5, color ="black"),
                plotlyOutput(ns("plot_1c"), height = 400)%>% withSpinner(type = 5, color ="black")#,
               # plotlyOutput(ns("plot_1d"), height = 400)%>% withSpinner(type = 5, color ="black")
                )
    )
  )
}