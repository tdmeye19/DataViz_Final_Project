library(tidyverse)
library(shiny)

ui <- fluidPage(
  titlePanel("MLS 2022 Season Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("clubchoice", 
                     label = "Choose a Club",
                     choices = statsfull$newclub2)),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("General Statistics", plotOutput("pointsplot")),
                tabPanel("Goal Leaders", tableOutput("goalstable")),
                tabPanel("Assist Leaders", tableOutput("assiststable")))
    
  )
  )
)


server <- function(input, output, session) {
  stats_sub <- reactive({
    statsfull %>% filter(newclub2 == input$clubchoice)
  })
  
  goals_sub <- reactive({
    goal_leaders %>% filter(Club == input$clubchoice)
  })
  
  assists_sub <- reactive({
    assist_leaders %>% filter(Club == input$clubchoice)
  })
  
  pointsplot <- reactive({
    ggplot(data = statsfull, aes(x = Points, y = `Goal Differential`)) +
      geom_point() +
      coord_flip() +
      geom_point(data = stats_sub(), aes(x = Points, y = `Goal Differential`, colour = "red"))
  })
  
  output$pointsplot <- renderPlot(
    pointsplot()
  )
  
  output$goalstable <- renderTable(
    goals_sub()
  )
  
  output$assiststable <- renderTable(
    assists_sub()
  )
}

shinyApp(ui, server)
