library(tidyverse)
library(shiny)
library(ggrepel)
library(zoo)
library(shinythemes)
library(shinydashboard)
library(ggthemes)

# install.package('remotes')
# remotes::install_github('coolbutuseless/emphatic')



ui <- fluidPage(
  theme = shinytheme(theme = "superhero"),
  titlePanel("Major League Soccer (MLS) 2022 Season Statistics"),
  
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
                         tableOutput("conftable"), textOutput("confdesc")),
                tabPanel("General Statistics", a(href ="https://www.espn.com/soccer/table/_/league/usa.1", "MLS Standings"),
                         plotOutput("pointsplot")),
                tabPanel("Goal Leaders", a(href = "https://www.espn.com/soccer/stats/_/league/usa.1", "Scoring Statistics"),
                         tableOutput("goalstable"), textOutput("goaldesc")),
                tabPanel("Assist Leaders", a(href = "https://www.espn.com/soccer/stats/_/league/usa.1", "Scoring Statistics"),
                         tableOutput("assiststable"), textOutput("assistdesc")),
                tabPanel("Club Value", a(href = "https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1/plus/?stichtag=2021-03-15", "TransferMarkt"),
                         br(), br(),
                         textOutput("valueexplan"),
                         a(href = "https://www.transfermarkt.co.in/transfermarkt-market-value-explained-how-is-it-determined-/view/news/385100#:~:text=Transfermarkt%20does%20not%20use%20an,and%20situational%20parameters%20outlined%20below.", "How TransferMarkt Determines Player Value"),
                         br(), br(),
                         plotOutput("valueplot"), br(), br(), plotOutput("valuegfplot")),
                ## tabPanel("Club Value & Goals For", a(href = "https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1/plus/?stichtag=2021-03-15", "TransferMarkt"),
                         ## plotOutput("valuegfplot")),
                tabPanel("Expected Points", a(href = "https://app.americansocceranalysis.com/#!/mlsnp/xgoals/teams", "American Soccer Analysis"),
                         plotOutput("xpointsplot")),
                tabPanel("Expected Goals", a(href = "https://app.americansocceranalysis.com/#!/mlsnp/xgoals/teams", "American Soccer Analysis"), 
                                             plotOutput("xgoalsplot")),
                tabPanel("Keeper Statistics", a(href = "https://app.americansocceranalysis.com/#!/mlsnp/xgoals/goalkeepers", "American Soccer Analysis"), 
                                                fluidRow(column(6, plotOutput("keeperplot")),
                                                       column(6, plotOutput("keeperbox")))),
                tabPanel("Player Expected Goals", a(href = "https://app.americansocceranalysis.com/#!/mls/xgoals/players", "American Soccer Analysis")
                         ,plotOutput("playerplot")),
                tabPanel("Data Sources", a(href ="https://www.espn.com/soccer/table/_/league/usa.1", "MLS Standings"),
                         br(),
                         a(href = "https://www.espn.com/soccer/stats/_/league/usa.1", "Scoring Statistics"),
                         br(),
                         a(href = "https://www.transfermarkt.us/major-league-soccer/marktwerteverein/wettbewerb/MLS1/plus/?stichtag=2021-03-15", "TransferMarkt"),
                         br(),
                         a(href = "https://app.americansocceranalysis.com/#!/mlsnp/xgoals/teams", "American Soccer Analysis"),
                         br(), br(),
                         a(href = "https://github.com/tdmeye19/DataViz_Final_Project", "Project GitHub")
                )))))
                


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
      select(newrank, Player, Club, `Games Played`, Goals) %>%
      rename("Rank" = newrank)
    
  })
  
  assists_sub <- reactive({
    assist_leaders %>% filter(Club == input$clubchoice) %>% 
      select(newrank, Player, Club, `Games Played`, Assists) %>%
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
      labs(title = "Are Clubs who Score More Winning?",
            x = "Points",
            y = "Goals For") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text = element_text(size = 18)) +
      scale_colour_brewer(palette = "Dark2")
    
  })
  
  valueplot <- reactive({
    ggplot(data = statsfull, aes(x = value22, y = Points, colour = Conference)) +
      geom_point(data = conf_sub(), size = 4) +
      geom_point(data = notconf_sub(), alpha = 0.35, size = 4) +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 6, shape = 1) +
      labs(title = "Do Clubs who have More Valuable Players Win More?",
           subtitle = "Dotted Line is Average Club Value",
           x = "Club Value (in millions of dollars)",
           y = "Points") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      geom_vline(xintercept = 39.3, linetype = "dashed") +
      scale_colour_brewer(palette = "Dark2")
  })
  
  valuegfplot <- reactive({
    ggplot(data = statsfull, aes(x = value22, y = GF, colour = Conference)) +
      geom_point(data = conf_sub(), size = 4) +
      geom_point(data = notconf_sub(), alpha = 0.35, size = 4) +
      coord_flip() +
      geom_label_repel(data = stats_sub(), aes(label = input$clubchoice)) +
      geom_point(data = stats_sub(), size = 6, shape = 1) +
      labs(title = "Do Clubs who have More Valuable Players Score More?",
           subtitle = "Dotted Line is Average Club Value",
           x = "Club Value (in millions of dollars)",
           y = "Goals For") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      geom_vline(xintercept = 39.3, linetype = "dashed") +
      scale_colour_brewer(palette = "Dark2")
  })
  
  xpointsplot <- reactive({
    statsfull %>% mutate(club = fct_reorder(club, xPts))
    ggplot(data = conf_sub(), aes(x = club, y = Pts)) +
      geom_point() + 
      geom_segment(data = conf_sub(), aes(x = club, xend = club, y = 0, yend = Pts)) +
      geom_segment(data = stats_sub(), aes(x = input$clubchoice, 
                                           xend = input$clubchoice, y = 0, 
                                           yend = Pts,
                                           colour = "Actual Points"), size = 0.8) +
      geom_point(data = conf_sub(), aes(x = club, y = xPts), colour = "red", alpha = 0.4) +
      geom_point(data = stats_sub(), aes(x = club, y = xPts, colour = "Expected Points")) +
      coord_flip() +
      labs(title = "Are Specific Clubs Meeting Their Expected Points?",
           x = "Club",
           y = "Points",
           colour = "Statistic") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_colour_brewer(palette = "Dark2")
      
  })
  
  xgoalsplot <- reactive({
    statsfull %>% mutate(club = fct_reorder(club, xGF))
    ggplot(data = conf_sub(), aes(x = club, y = GF)) +
      geom_point() +
      scale_colour_brewer(palette = "Dark2") +
      geom_segment(data = conf_sub(), aes(x = club, xend = club, y = 0, yend = GF)) +
      geom_segment(data = stats_sub(), aes(x = input$clubchoice, 
                                           xend = input$clubchoice, y = 0, yend = GF, 
                                           colour = "Actual Goals For"), size = 0.8) +
      geom_point(data = conf_sub(), aes(x = club, y = xGF), colour = "red", alpha = 0.4) +
      geom_point(data = stats_sub(), aes(x = club, y = xGF, colour = "Expected Goals For")) +
      coord_flip() +
      labs(title = "Are Specific Clubs Meeting Their Expected Goals?",
           x = "Club",
           y = "Goals For",
           colour = "Statistic") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 12))
  })
  
  keeperplot <- reactive({
    ggplot(data = goalie_df, aes(x = `xG`, y = `Goals Conceded`)) +
      geom_point() +
      scale_colour_brewer(palette = "Dark2") +
      geom_label_repel(data = keeper_sub(), aes(x = `xG`, y = `Goals Conceded`,
                                                label = Player)) +
      geom_point(data = keeper_sub(), size = 4, shape = 1) +
      labs(title = "Are Keepers Letting in More Goals than Expected?",
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
      scale_colour_brewer(palette = "Dark2") +
      geom_label_repel(data = player_sub(), aes(x = `xG`, y = G,
                                                label = Player, colour = Position)) +
      geom_abline(intercept = 0, slope = 1, colour = "red") +
      labs(title = "Are Specific Players Meeting their Expected Goals?",
           x = "Expected Goals",
           y = "Actual Goals Scored") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))
  })
  
  output$confdesc <- renderText("This table shows the standings for the conference that
                                was selected. A win is 3 points, a draw 1 point, and a loss 0 points.")
  
  output$goaldesc <- renderText("This table shows the goal leaders, for the club that was selected, if they
                                are in the top 50 goal scorers in MLS.")
  
  output$assistdesc <- renderText("This table shows the assist leaders, for the club that was selected,
                                  if they are in the top 50 for assists in MLS.")
  
  output$valueexplan <- renderText("Club value is based on transfer fees and market value for each of the players. For more information,
                                   follow the link below:")
  
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
      labs(title = "Which Conference Faces More Shots?",
           x = "Shots Faced (Season Total)",
           y = "Conference")
  )
  
  output$playerplot <- renderPlot(
    playerplot()
  )
  
  # output$playerinfo <- renderText({
  #   player_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("Player = ", player_df$Player, "Position = ", player_df$Position)
  #   }
  #   paste0(
  #     "Player =", player_str(input$plothover), "Position = ", player_df$Position
  #   )
  # })
  }

shinyApp(ui, server)
