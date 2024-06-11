fig4_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    f_plot_4a <- function() {
      d <- results_list$plot_4a_table %>% 
        left_join((results_list$plot_3_table %>%
                     mutate(tag = ifelse(my_label == "", paste0(label), my_label)) %>% 
                     select(topic,tag,level1) )) %>% 
        #add jitter coords
        mutate(point_x = ifelse(is.na(`Global topics`), `Regional topics`, `Global topics`) ) %>% 
        mutate(point_x = ifelse(is.na(tag), NA, point_x) )
      
      p <- ggplot(data =d,aes(x=x) ) +
        geom_density( aes(x = `Global topics`, y = ..density..), fill="#B2DF8A",alpha = .8 ) +
        annotate( "text",x=.5, y=.2, label="Global topics", color="black",size=3) +
        geom_density( aes(x = `Regional topics`, y = -..density..), fill= "#33A02C" ,alpha = .8 ) +
        annotate( "text",x=.5, y=-0.2, label="Regional topics", color="white",size=3) +
        geom_point(aes(x = point_x, y = 0, 
                       text = paste0('</br><b>',tag,'</b>',
                                     '</br><i>',level1,'</i>',
                                     '</br>Average normalized citations: ',round(point_x,2))), 
                   position = position_jitter(width = 0, height = 2),alpha = .1)+
        theme(text= element_text(size = 12))+
        theme_minimal()+
        labs(x = "Average normalized citations",  title = "A. Distribution of  field-normalized citations for each topic")+
        theme(axis.text.y = element_blank()) 
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    f_plot_4b <- function() {
      d <- results_list$plot_4b_table%>% 
        left_join((results_list$plot_3_table %>%
                     mutate(tag = ifelse(my_label == "", paste0(label), my_label)) %>% 
                     select(topic,tag,level1) )) %>% 
        #add jitter coords
        mutate(point_x = ifelse(is.na(`Global topics`), `Regional topics`, `Global topics`) ) %>% 
        mutate(point_x = ifelse(is.na(tag), NA, point_x) )
      
      p <- ggplot(data =d,aes(x=x) ) +
        geom_density( aes(x = `Global topics`, y = ..density..), fill="#B2DF8A",alpha = .8 ) +
        annotate( "text",x=0.2, y=.5, label="Global topics", color="black",size=3) +
        geom_density( aes(x = `Regional topics`, y = -..density..), fill= "#33A02C" ,alpha = .8 ) +
        annotate( "text",x=0.2, y=-.5, label="Regional topics", color="white",size=3)+
        geom_point(aes(x = point_x, y = 0, 
                       text = paste0('</br><b>',tag,'</b>',
                                     '</br><i>',level1,'</i>',
                                     '</br>Latin American citations: ',round(point_x*100,2),'%')), 
                   position = position_jitter(width = 0, height = 4),alpha = .1)+
        theme(text= element_text(size = 12))+
        theme_minimal()+
        scale_x_continuous(labels = function(x) paste0(x*100,"%"))+
        labs(x = "% of Latin American citations",y="density", title = "B. Distribution of the proportion of citations from Latin-American articles, for each topic")+
        theme(axis.text.y = element_blank()) 
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    f_plot_4c <- function() {
      d <- results_list$plot_4c_table
      
      p <-  ggplot(data =d,aes(x = fct_reorder(level1,order), y = citations, shape = group_legend,color =group_legend,group = Gender,
                               text = paste0('</br><b>',group_legend,'</b>',
                                             '</br>',level1,
                                             '</br>',round(citations,2),
                                             '</br>95% Confidence interval: (', round(lower_ci,2), ' ; ',round(upper_ci,2), ')')
                               ))+
        geom_point(size = 3, alpha = 1,position=position_dodge(width =.2))+
        theme(text= element_text(size = 12))+
        theme_minimal()+
        scale_color_manual(values = c("#A6CEE3","#A6CEE3","#1F78B4","#1F78B4"))+
        labs(color = "", shape = "", 
             x = "", y = "Average normalized citations",
             title = "C. Average field-normalized citations of publications \n by discipline, gender and dissemination circuit")+
        scale_shape_manual( values = c(16,17,16,17))+
        scale_x_discrete(labels=function(x) str_wrap(x,15))+
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = "text")%>% layout(font = list(family = "Arial"))
    }
    
    output$plot_4a <- renderPlotly({
      f_plot_4a()
    })
    output$plot_4b <- renderPlotly({
      f_plot_4b()
    })
    output$plot_4c <- renderPlotly({
      f_plot_4c()
    })
    
  })
}

fig4_plot_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Impact",
    sidebarLayout(
      sidebarPanel(
        width = 0
      ),
      mainPanel(h2("Scholarly impact, research topics and publication venue"),
                h4("1993-2022"),
                br(),
                br(),
                plotlyOutput(ns("plot_4a"), height = 400)%>% withSpinner(type = 5, color ="black"),
                br(),
                plotlyOutput(ns("plot_4b"), height = 400)%>% withSpinner(type = 5, color ="black"),
                br(),
                plotlyOutput(ns("plot_4c"), height = 500)%>% withSpinner(type = 5, color ="black")
                )
    )
  )
}