library(tidyverse)
library(shiny)
library(ggrepel)
library(zoo)
library(shinythemes)
library(shinydashboard)

# install.package('remotes')
# remotes::install_github('coolbutuseless/emphatic')



ui <- fluidPage(
  theme = shinytheme(theme = "superhero"),
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
                tabPanel("Expected Goals", plotOutput("xgoalsplot")),
                tabPanel("Keeper Statistics", fluidRow(12,
                                                       column(6, plotOutput("keeperplot")),
                                                       column(6, plotOutput("keeperbox")))
)))


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
  
  keeper_sub <- reactive({
    goalie_df %>% filter(club == input$clubchoice)
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
      geom_point(size = 4) +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 6, shape = 1) +
      scale_colour_brewer(palette = "Dark2") +
      labs(title = "Points Comparison to Goal Differential",
            x = "Points",
            y = "Goal Differential") +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      theme_bw() +
      geom_hline(yintercept = 0, linetype = "dashed")
  })
  
  valueplot <- reactive({
    ggplot(data = statsfull, aes(x = value22, y = Points, colour = Conference)) +
      geom_point(data = conf_sub(), size = 4) +
      geom_point(data = notconf_sub(), alpha = 0.35, size = 4) +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 6, shape = 1) +
      scale_colour_brewer(palette = "Dark2") +
      labs(title = "Club Value Compared to Points",
           caption = "Data Source: https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1",
           x = "Club Value (in millions of dollars)",
           y = "Points") +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      theme_bw() 
      #geom_hline(xintercept = mean(x))
  })
  
  valuegfplot <- reactive({
    ggplot(data = statsfull, aes(x = value22, y = GF, colour = Conference)) +
      geom_point(data = conf_sub(), size = 4) +
      geom_point(data = notconf_sub(), alpha = 0.35, size = 4) +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 6, shape = 1) +
      scale_colour_brewer(palette = "Dark2") +
      labs(title = "Club Value Compared to Goals For",
           caption = "Data Source: https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1",
           x = "Club Value (in millions of dollars)",
           y = "Goals For") +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 14)) +
        theme_bw()
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
           caption = "Data Source: https://app.americansocceranalysis.com/#!/mls",
           x = "Club",
           y = "Points",
           colour = "Statistic") +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14)) +
      theme_bw()
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
           caption = "Data Source: https://app.americansocceranalysis.com/#!/mls",
           x = "Club",
           y = "Goals For",
           colour = "Statistic") +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14)) +
      theme_bw()
  })
  
  keeperplot <- reactive({
    ggplot(data = goalie_df, aes(x = `xG`, y = `Goals Conceded`)) +
      geom_point() +
      scale_colour_brewer(palette = "Dark2") +
      geom_label_repel(data = keeper_sub(), aes(x = `xG`, y = `Goals Conceded`,
                                                label = Player)) +
      geom_point(data = keeper_sub(), size = 6, shape = 1) +
      labs(title = "Expected Goals Compared to Goals Conceded",
           subtitle = "Red Line is Allowing the Expected Number of Goals, 
           Keepers Below the Line are Letting in Less Shots than Expected",
           caption = "Data Source: https://app.americansocceranalysis.com/#!/mls/xgoals/goalkeepers",
           x = "Expected Goals",
           y = "Goals Conceded") +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14)) +
      theme_bw() +
      geom_abline(intercept = 0, slope = 1, colour = "red")
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
  
  output$keeperplot <- renderPlot(
    keeperplot()
  )
  
  output$keeperbox <- renderPlot(
    ggplot(data = goalie_df, aes(x = `Shots Faced`, y = Conference, colour = Conference)) +
      geom_boxplot() + coord_flip() + theme_bw()
  )
}

shinyApp(ui, server)
