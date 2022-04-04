library(tidyverse)
library(shiny)
library(ggrepel)
library(zoo)
library(emphatic)
# install.package('remotes')
# remotes::install_github('coolbutuseless/emphatic')

ui <- fluidPage(
  titlePanel("MLS 2022 Season Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("confchoice", 
                     label = "Choose a Conference",
                     choices = statsfull$Conference),
      selectizeInput("clubchoice", 
                     label = "Choose a Club",
                     choices = statsfull$newclub2,
                     selected = NULL)),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Conference Table", tableOutput("conftable")),
                tabPanel("General Statistics", plotOutput("pointsplot")),
                tabPanel("Goal Leaders", tableOutput("goalstable")),
                tabPanel("Assist Leaders", tableOutput("assiststable")),
                tabPanel("Club Value"),
                tabPanel("Expected Statistics"))
    
  )
  )
)


server <- function(input, output, session) {
  stats_sub <- reactive({
    statsfull %>% filter(newclub2 == input$clubchoice)
  })
  
  conf_sub <- reactive({
    statsfull %>% filter(Conference == input$confchoice)
  })
  
  club_sub <- reactive({
    conf_sub() %>% select(newclub2)
  })
  
  observeEvent(input$confchoice, {
    updateSelectizeInput(inputId = "clubchoice", choices = club_sub())
  })
  
  goals_sub <- reactive({
    goal_leaders %>% filter(Club == input$clubchoice) %>% select(newrank, Player, Club, GP, Goals)
  })
  
  assists_sub <- reactive({
    assist_leaders %>% filter(Club == input$clubchoice) %>% select(newrank, Player, Club, GP, Assists)
  })
  
  pointsplot <- reactive({
    ggplot(data = statsfull, aes(x = Points, y = `Goal Differential`, colour = Conference)) +
      geom_point() +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 3, shape = 1) +
      scale_colour_brewer(palette = "Dark2")
  })
  
  output$pointsplot <- renderPlot(
    pointsplot()
  )
  
  output$conftable <- renderTable(
    conf_sub()
  )
  
  output$goalstable <- renderTable(
    goals_sub() 
  )
  
  output$assiststable <- renderTable(
    assists_sub()
  )
}

shinyApp(ui, server)
