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
                tabPanel("Conference Table", a(href ="https://www.espn.com/soccer/table/_/league/usa.1", "MLS Standings"),
                         tableOutput("conftable")),
                tabPanel("General Statistics", a(href ="https://www.espn.com/soccer/table/_/league/usa.1", "MLS Standings"),
                         plotOutput("pointsplot")),
                tabPanel("Goal Leaders", a(href = "https://www.espn.com/soccer/stats/_/league/usa.1", "Scoring Statistics"),
                         tableOutput("goalstable")),
                tabPanel("Assist Leaders", a(href = "https://www.espn.com/soccer/stats/_/league/usa.1", "Scoring Statistics"),
                         tableOutput("assiststable")),
                tabPanel("Club Value & Points", a(href = "https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1/plus/?stichtag=2021-03-15", "TransferMarkt"),
                         plotOutput("valueplot")),
                tabPanel("Club Value & Goals For", a(href = "https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1/plus/?stichtag=2021-03-15", "TransferMarkt"),
                         plotOutput("valuegfplot")),
                tabPanel("Expected Points", a(href = "https://app.americansocceranalysis.com/#!/mlsnp/xgoals/teams", "American Soccer Analysis"),
                         plotOutput("xpointsplot")),
                tabPanel("Expected Goals", a(href = "https://app.americansocceranalysis.com/#!/mlsnp/xgoals/teams", "American Soccer Analysis"), 
                                             plotOutput("xgoalsplot")),
                tabPanel("Keeper Statistics", a(href = "https://app.americansocceranalysis.com/#!/mlsnp/xgoals/goalkeepers", "American Soccer Analysis"), 
                                                fluidRow(column(6, plotOutput("keeperplot")),
                                                       column(6, plotOutput("keeperbox")))),
                tabPanel("Player Expected Goals", plotOutput("playerplot"))# , hover = "plothover"))
                ))))


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
  
  player_sub <- reactive({
    player_df %>% filter(club == input$clubchoice)
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
    ggplot(data = statsfull, aes(x = Points, y = `Goals For`,
                                 colour = Conference)) +
      geom_point(size = 4) +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 6, shape = 1) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_colour_brewer(palette = "Dark2") +
      labs(title = "Points Comparison to Goals For",
            x = "Points",
            y = "Goals For") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text = element_text(size = 18)) 
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
           x = "Club Value (in millions of dollars)",
           y = "Points") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      geom_vline(xintercept = 39.3, linetype = "dashed")
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
           x = "Club Value (in millions of dollars)",
           y = "Goals For") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      geom_vline(xintercept = 39.3, linetype = "dashed")
  })
  
  xpointsplot <- reactive({
    statsfull %>% mutate(club = fct_reorder(club, xPts))
    ggplot(data = statsfull, aes(x = club, y = Pts)) +
      geom_point() + 
      geom_segment(data = statsfull, aes(x = club, xend = club, y = 0, yend = Pts)) +
      geom_segment(data = stats_sub(), aes(x = input$clubchoice, 
                                           xend = input$clubchoice, y = 0, 
                                           yend = Pts,
                                           colour = "Actual Points"
                                           )) +
      geom_point(data = stats_sub(), aes(x = club, y = xPts, colour = "Expected Points")) +
      coord_flip() +
      scale_colour_brewer(palette = "Dark2") +
      labs(title = "Expected Points Compared to Actual Points",
           x = "Club",
           y = "Points",
           colour = "Statistic") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14))
      
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
           x = "Club",
           y = "Goals For",
           colour = "Statistic") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14))
  })
  
  keeperplot <- reactive({
    ggplot(data = goalie_df, aes(x = `xG`, y = `Goals Conceded`)) +
      geom_point() +
      scale_colour_brewer(palette = "Dark2") +
      geom_label_repel(data = keeper_sub(), aes(x = `xG`, y = `Goals Conceded`,
                                                label = Player)) +
      geom_point(data = keeper_sub(), size = 4, shape = 1) +
      labs(title = "Expected Goals Compared to Goals Conceded",
           subtitle = "Red Line is Allowing the Expected Number of Goals",
           x = "Expected Goals",
           y = "Goals Conceded") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14)) +
      geom_abline(intercept = 0, slope = 1, colour = "red")
  })
  
  playerplot <- reactive({
    ggplot(data = player_sub(), aes(x = xG, y = G)) +
      geom_point() +
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
      geom_boxplot() + coord_flip() + theme_bw() +
      scale_colour_brewer(palette = "Dark2") +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14)) +
      labs(title = "Shots Faced in Eastern vs. Western Conference",
           x = "Shots Faced (Season Total)",
           y = "Conference")
  )
  
  output$playerplot <- renderPlot(
    playerplot()
  )
}

shinyApp(ui, server)
