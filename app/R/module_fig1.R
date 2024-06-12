fig1_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    f_plot_1a <- function() {
      d <- results_list$plot_1a_table
      
      p <- ggplot(data=d,aes(y=value,x=as.numeric(pub_year),group = ind,color = gender, linetype = Circuit,
                             text = paste0('</br>',gender, ' - ', Circuit,
                                           '</br>Number of publications: ',round(value,0),
                                           '</br>Publication year: ',pub_year)
                             ))+
        geom_line()+
        theme_minimal()+
        theme(legend.position = "bottom")+
        theme(text = element_text(size =test_size_fig1))+
        scale_color_brewer(palette = "Paired",direction = 1)+
        labs(y = "Number of publications",
             x = "Publication year", fill = "", color = "", linetype = "",
             title= "A. Number of publications by gender and circuit for Latin-American researchers")+
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = "text")%>% 
        layout(font = list(family = "Arial"))
    }
    
    f_plot_1b <- function() {
      d <- results_list$plot_1b_table
      
      p <- ggplot(data =d,aes(y=value,x=as.numeric(pub_year),group = ind,color = ind, linetype = ind,
                              text = paste0('</br>',ind,
                                            '</br>',round(value*100,2),'%',
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
             title = "B. Proportion of Latin-American distinct women authors and authorships"
        )+
        guides(fill = guide_legend(reverse=TRUE))+
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    f_plot_1c <- function() {
      d <- results_list$plot_1c_table
      
      p <- ggplot(data=d,aes(y=prop,x=as.numeric(pub_year),color = ind,group=ind ,linetype = ind ,
                             text = paste0('</br>',ind,
                                           '</br>Published in Latin America: ',round(prop*100,2),'%',
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
             title= "C. Proportion of documents published by Latin-American researchers \n that appear in Latin-American journals or conferences")+
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    f_plot_1d_1 <- function() {
      d <- results_list$plot_1d_table

      p <- ggplot(data = d,aes(x=as.numeric(pub_year)))+
        geom_point(aes(y = `Women authors wrt women authorships`
                       ,text = paste0('</br>Women authors wrt women authorships: ', round(`Women authors wrt women authorships`*100,2),'%',
                                      '</br>Publication year: ',pub_year)
                       ),size=.2,color = "#6A3D9A")+
        
        geom_smooth(aes(y = `Women authors wrt women authorships`), size=.5,color = "#6A3D9A",fill = "lightgray")+
       
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        theme_minimal()+
        theme(text = element_text(size = test_size_fig1))+
        theme(axis.text.x = element_text(size =test_size_fig1))+
        theme(axis.text.y = element_text(size =test_size_fig1))+
        theme(legend.text = element_text(size =test_size_fig1))+
        #scale_color_viridis(discrete = TRUE,option = "F", begin = .2, end =.5)+
        labs(x = "Publication year", 
             title = "D. Gap between Latin-American women distinct authors and authorships, \n and gender productivity gap"
        )


      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    f_plot_1d_2 <- function() {
      d <- results_list$plot_1d_table
      
      p <- ggplot(data = d,aes(x=as.numeric(pub_year)))+
       
        geom_point(aes(y = `Productivity gap`
                       ,text = paste0('</br>Productivity gap: ', round(`Productivity gap`*100,2),'%',
                                      '</br>Publication year: ',pub_year)
                       ),size=.2,color = "#33A02C")+
        
        geom_smooth(aes(y = `Productivity gap`), size=.5,color = "#33A02C",fill = "lightgray")+
        
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
     
        theme_minimal()+
        theme(text = element_text(size = test_size_fig1))+
        theme(axis.text.x = element_text(size =test_size_fig1))+
        theme(axis.text.y = element_text(size =test_size_fig1))+
        theme(legend.text = element_text(size =test_size_fig1))+
        #scale_color_viridis(discrete = TRUE,option = "F", begin = .2, end =.5)+
        labs(x = "Publication year", 
             title = ""
        )
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    output$plot_1a <- renderPlotly({
      f_plot_1a()
    })
    output$plot_1b <- renderPlotly({
      f_plot_1b()
    })
    output$plot_1c <- renderPlotly({
      f_plot_1c()
    })
    output$plot_1d_1 <- renderPlotly({
      f_plot_1d_1()
    })
    output$plot_1d_2 <- renderPlotly({
      f_plot_1d_2()
    })
    
    
  })
}

fig1_plot_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Context",
    sidebarLayout(
      sidebarPanel(
        width = 0
      ),
      mainPanel(h2("Gendered research dissemination circuits in Latin America"),
                h4("1993-2022"),
                br(),
                br(),
                plotlyOutput(ns("plot_1a"), height = 400)%>% withSpinner(type = 5, color ="black"),
                br(),
                plotlyOutput(ns("plot_1b"), height = 400)%>% withSpinner(type = 5, color ="black"),
                br(),
                plotlyOutput(ns("plot_1c"), height = 400)%>% withSpinner(type = 5, color ="black"),
                br(),
                plotlyOutput(ns("plot_1d_1"), height = 400)%>% withSpinner(type = 5, color ="black"),
                br(),
                plotlyOutput(ns("plot_1d_2"), height = 400)%>% withSpinner(type = 5, color ="black")
                )
    )
  )
}