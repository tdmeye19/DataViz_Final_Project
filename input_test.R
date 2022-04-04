library(shiny)

ui <- fluidPage(
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
                  tabPanel("Conference Table", tableOutput("conftable")
    )
))))


server <- function(input, output, session) {
  vals <- reactiveValues(
    row_priority = statsfull$newclub2,
    row_color = rep('white', 3)
  )
  
  observeEvent(input$clubchoice, {
    vals$row_priority <- 
      c(input$clubchoice, vals$row_priority[vals$row_priority != input$clubchoice])
    vals$row_color <- c('lightgreen', 'white', 'white')
  })
  conf_sub <- reactive({
    statsfull %>% filter(Conference == input$confchoice)
  })
  
  observeEvent(input$confchoice, {
    updateSelectizeInput(inputId = "clubchoice", choices = conf_sub())
  })
  
  conf_sub <- reactive({
    statsfull %>% filter(Conference == input$confchoice)
  })
  
  club_sub <- reactive({
    conf_sub() %>% select(newclub2) %>% 
      formatStyle("clubchoice",
                  target = "row",
                  backgroundColor = styleEqual(vals$row_priority, 
                  vals$row_color, 
                  default = 'white'))
  })
  
  output$conftable <- renderTable(
    conf_sub()
  )
}

shinyApp(ui, server)
