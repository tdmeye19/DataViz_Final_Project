library(tidyverse)
library(shiny)
library(ggrepel)
library(zoo)
library(emphatic)
# install.package('remotes')
# remotes::install_github('coolbutuseless/emphatic')

## I am fixing the committ message

ui <- fluidPage(
  titlePanel("MLS 2022 Season Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("confchoice", 
                     label = "Choose a Conference",
                     choices = statsfull$Conference),
      selectizeInput("clubchoice", 
                     label = "Choose a Club",
                     choices = statsfull$club,
                     selected = NULL)),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Conference Table", tableOutput("conftable")),
                tabPanel("General Statistics", plotOutput("pointsplot")),
                tabPanel("Goal Leaders", tableOutput("goalstable")),
                tabPanel("Assist Leaders", tableOutput("assiststable")),
                tabPanel("Club Value & Points", plotOutput("valueplot")),
                tabPanel("Club Value & Goals For", plotOutput("valuegfplot")),
                tabPanel("Expected Points", plotOutput("xpointsplot")),
                tabPanel("Expected Goals", plotOutput("xgoalsplot")))
    
  )
  )
)


server <- function(input, output, session) {
  stats_sub <- reactive({
    statsfull %>% filter(club == input$clubchoice)
  })
  
  conf_sub <- reactive({
    statsfull %>% filter(Conference == input$confchoice)
  })
  
  notconf_sub <- reactive({
    statsfull %>% filter(Conference != input$confchoice)
  })
  
  club_sub <- reactive({
    conf_sub() %>% select(club)
  })
  
  observeEvent(input$confchoice, {
    updateSelectizeInput(inputId = "clubchoice", choices = club_sub())
  })
  
  goals_sub <- reactive({
    goal_leaders %>% filter(Club == input$clubchoice) %>% 
      select(newrank, Player, Club, GP, Goals) %>%
      rename("Rank" = newrank)
    
  })
  
  assists_sub <- reactive({
    assist_leaders %>% filter(Club == input$clubchoice) %>% 
      select(newrank, Player, Club, GP, Assists) %>%
      rename("Rank" = newrank)
  })
  
  pointsplot <- reactive({
    ggplot(data = statsfull, aes(x = Points, y = `Goal Differential`,
                                 colour = Conference)) +
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
           caption = "Data Source: https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1",
           x = "Club Value (in millions of dollars)",
           y = "Points")
  })
  
  valuegfplot <- reactive({
    ggplot(data = statsfull, aes(x = value22, y = GF, colour = Conference)) +
      geom_point(data = conf_sub()) +
      geom_point(data = notconf_sub(), alpha = 0.35) +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 3, shape = 1) +
      scale_colour_brewer(palette = "Dark2") +
      labs(title = "Club Value Compared to Goals For",
           caption = "Data Source: https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1",
           x = "Club Value (in millions of dollars)",
           y = "Goals For")
  })
  
  xpointsplot <- reactive({
    statsfull %>% mutate(club = fct_reorder(club, xPts))
    ggplot(data = statsfull, aes(x = club, y = Pts)) +
      geom_point() + 
      geom_segment(data = statsfull, aes(x = club, xend = club, y = 0, yend = Pts)) +
      geom_segment(data = stats_sub(), aes(x = input$clubchoice, 
                                           xend = input$clubchoice, y = 0, 
                                           yend = Pts,
                                           colour = "Actual Points")) +
      geom_point(data = stats_sub(), aes(x = club, y = xPts, colour = "Expected Points")) +
      coord_flip() +
      scale_colour_brewer(palette = "Dark2") +
      labs(title = "Expected Points Compared to Actual Points",
           subtitle = "Expected Points is in Red",
           caption = "Data Source: https://app.americansocceranalysis.com/#!/mls",
           x = "Club",
           y = "Points",
           colour = "Statistic")
  })
  
  xgoalsplot <- reactive({
    statsfull %>% mutate(club = fct_reorder(club, xGF))
    ggplot(data = statsfull, aes(x = club, y = GF)) +
      geom_point() +
      scale_colour_brewer(palette = "Dark2") +
      geom_segment(data = statsfull, aes(x = club, xend = club, y = 0, yend = GF)) +
      geom_segment(data = stats_sub(), aes(x = input$clubchoice, 
                                           xend = input$clubchoice, y = 0, yend = GF, 
                                           colour = "Actual Goals For")) +
      geom_point(data = stats_sub(), aes(x = club, y = xGF, colour = "Expected Goals For")) +
      coord_flip() +
      labs(title = "Expected Goals Compared to Actual Goals",
           subtitle = "Expected Goals For is in Red",
           caption = "Data Source: https://app.americansocceranalysis.com/#!/mls",
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
  
  output$valuegfplot <- renderPlot(
    valuegfplot()
  )
  
  output$xpointsplot <- renderPlot(
    xpointsplot()
  )
  
  output$xgoalsplot <- renderPlot(
    xgoalsplot()
  )
}

shinyApp(ui, server)
