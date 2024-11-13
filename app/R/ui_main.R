library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(gt)
library(markdown)

# load('data.Rdata')

about_ui <- tabPanel(
  title = "Home",
  titlePanel('Science for whom?'),
  h3('The influence of the regional academic circuit on gender inequalities in Latin America'),
  HTML('&nbsp;'),
  HTML('&nbsp;'),
  
  tags$div(
    style = "width: 600px; text-align: justify;",
  h4("About the app"),
  p("Our app is designed to help you further explore and understand our results.
    To read the full research paper, visit this", a(href ='https://arxiv.org/abs/2407.18783',"site")) 
  ),
  #update publication site!!!
  

  
  h5("Contents"),
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('1. The Latin-American context')),
    onclick="fakeClick('Context')",
    style ="text-decoration: none !important;"
  ),
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('2. Disciplines and dissemination circuits by gender')),
    onclick="fakeClick('Disciplines')",
    style ="text-decoration: none !important;"
  ),
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('3. The topic space of science')),
    onclick="fakeClick('Topics')",
    style ="text-decoration: none !important;"
  ),
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('4. Scholarly impact and dissemination circuits')),
    onclick="fakeClick('Impact')",
    style ="text-decoration: none !important;"
  ),
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('6. Methods')),
    onclick="fakeClick('Methods')",
    style ="text-decoration: none !important;"
  ),
  
  HTML('&nbsp;'),
  HTML('&nbsp;'),
  
  tags$div(
    style = "width: 600px; text-align: justify;",
  h4("What is this paper about?"),
  p(abstract)
  )
  

  
)

methods <- tabPanel(
  title = "Methods",
  h4("Dataset and sources"),
  tags$div(
    style = "width: 600px; text-align: justify;",
  p("Data for this article were retrieved from the",a(href ='https://www.dimensions.ai/',"Dimensions"),
  "database. We examine all publications with at least one Latin America-affiliated author between 1993 and 2022, based on the first institutional affiliation of each author. 
  ")),
 
    h4("Gender inference"),
  tags$div(
    style = "width: 600px; text-align: justify;",
    p("The metadata retrieved includes authors’ given and family names, which are used to infer gender. 
    The gender disambiguation algorithm builds on the method presented in",
    a(href = 'https://www.nature.com/articles/504211a',"Larivière et al. (2013)"), "and" ,
    a(href = 'https://www.hup.harvard.edu/books/9780674919297',"Sugimoto and Larivière (2023)"), 
    "which uses census data and country-specific lists of men and women names to assign probable gender to researchers based on given names and family names.
  ")),
  
      h4("Academic circuits"),
  tags$div(
    style = "width: 600px; text-align: justify;",
    p("We operationalized insertion in the regional academic circuit through publication in a journal located in Latin America (for articles) or publication at a conference held in Latin America (for conference proceedings).
  ")),
    
      h4("Topic model"),
  tags$div(
    style = "width: 600px; text-align: justify;",
  p("We used articles’ abstracts and titles to train a multilingual semi-supervised", 
    a(href = 'https://maartengr.github.io/BERTopic/index.html',"BERTopic"), "model to infer the topic of scientific publications. 
  We considered 100 publications as the minimum topic size.
  ")),
    
      h4("Impact analysis"),
  tags$div(
    style = "width: 600px; text-align: justify;",
    p("Scholarly impact is assessed through field- and year-normalized citations, 
    using all citations received by articles in our corpus until the end of 2022.
  "))
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