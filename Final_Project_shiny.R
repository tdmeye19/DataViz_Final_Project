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
  
  notconf_sub <- reactive({
    statsfull %>% filter(Conference != input$confchoice)
  })
  
  club_sub <- reactive({
    conf_sub() %>% select(newclub2)
  })
  
  observeEvent(input$confchoice, {
    updateSelectizeInput(inputId = "clubchoice", choices = club_sub())
  })
  
  goals_sub <- reactive({
    goal_leaders %>% filter(Club == input$clubchoice) %>% select(newrank, Player, Club, GP, Goals) %>%
      rename("Rank" = newrank)
    
  })
  
  assists_sub <- reactive({
    assist_leaders %>% filter(Club == input$clubchoice) %>% select(newrank, Player, Club, GP, Assists) %>%
      rename("Rank" = newrank)
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
    ggplot(data = statsfull, aes(x = value22, y = Points, colour = Conference)) +
      geom_point(data = conf_sub()) +
      geom_point(data = notconf_sub(), alpha = 0.35) +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 3, shape = 1) +
      scale_colour_brewer(palette = "Dark2") +
      labs(title = "Club Value Compared to Points",
           subtitle = "https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1",
           x = "Club Value",
           y = "Points")
  })
  
  xpointsplot <- reactive({
    statsfull %>% mutate(newclub2 = fct_reorder(newclub2, xPts))
    ggplot(data = statsfull, aes(x = newclub2, y = Pts)) +
      geom_point() + 
      geom_segment(data = statsfull, aes(x = newclub2, xend = newclub2, y = 0, yend = Pts)) +
      geom_segment(data = stats_sub(), aes(x = input$clubchoice, xend = input$clubchoice, y = 0, yend = Pts, colour = "green")) +
      geom_point(data = stats_sub(), aes(x = newclub2, y = xPts, colour = "red")) +
      coord_flip() +
      labs(title = "Expected Points Compared to Actual Points",
           subtitle = "https://app.americansocceranalysis.com/#!/mls",
           x = "Club",
           y = "Points",
           colour = "Statistic")
  })
  
  xgoalsplot <- reactive({
    statsfull %>% mutate(newclub2 = fct_reorder(newclub2, xGF))
    ggplot(data = statsfull, aes(x = newclub2, y = GF)) +
      geom_point() + 
      geom_segment(data = statsfull, aes(x = newclub2, xend = newclub2, y = 0, yend = GF)) +
      geom_segment(data = stats_sub(), aes(x = input$clubchoice, xend = input$clubchoice, y = 0, yend = GF, colour = "green")) +
      geom_point(data = stats_sub(), aes(x = newclub2, y = xGF, colour = "red")) +
      coord_flip() +
      labs(title = "Expected Goals Compared to Actual Goals",
           subtitle = "https://app.americansocceranalysis.com/#!/mls",
           x = "Club",
           y = "Goals For",
           colour = "Statistic")
  })
  
  output$pointsplot <- renderPlot(
    pointsplot()
  )
  
  output$conftable <- renderTable(
    conf_sub() %>% select(1:11)
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
