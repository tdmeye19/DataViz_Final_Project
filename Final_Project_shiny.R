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
                tabPanel("Club Value", plotOutput("valueplot")),
                tabPanel("Expected Points", plotOutput("xpointsplot")),
                tabPanel("Expected Goals", plotOutput("xgoalsplot")))
    
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
      scale_colour_brewer(palette = "Dark2") +
      labs(title = "Points Comparison to Goal Differential",
            x = "Points",
            y = "Goal Differential")
  })
  
  valueplot <- reactive({
    ggplot(data = statsfull, aes(x = value22, y = Points, colour = diffind)) +
      geom_point() +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 3, shape = 1) +
      scale_colour_brewer(palette = "Dark2")
  })
  
  xpointsplot <- reactive({
    ggplot(data = statsfull, aes(x = newclub2, y = Pts)) +
      geom_point() + 
      geom_segment(data = statsfull, aes(x = newclub2, xend = newclub2, y = 0, yend = Pts)) +
      geom_segment(data = stats_sub(), aes(x = input$clubchoice, xend = input$clubchoice, y = 0, yend = Pts, colour = "green")) +
      geom_point(data = stats_sub(), aes(x = newclub2, y = xPts, colour = "red")) +
      coord_flip()
  })
  
  xgoalsplot <- reactive({
    ggplot(data = statsfull, aes(x = newclub2, y = GF)) +
      geom_point() + 
      geom_segment(data = statsfull, aes(x = newclub2, xend = newclub2, y = 0, yend = GF)) +
      geom_segment(data = stats_sub(), aes(x = input$clubchoice, xend = input$clubchoice, y = 0, yend = GF, colour = "green")) +
      geom_point(data = stats_sub(), aes(x = newclub2, y = xGF, colour = "red")) +
      coord_flip()
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
  
  output$valueplot <- renderPlot(
    valueplot()
  )
  
  output$xpointsplot <- renderPlot(
    xpointsplot()
  )
  
  output$xgoalsplot <- renderPlot(
    xgoalsplot()
  )
}

shinyApp(ui, server)
