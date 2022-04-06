library(shiny)

club_list <- statsfull$newclub2
dat <- data.frame(club = club_list,
                  seedval = 1:28)

ui <- fluidPage(
  selectizeInput("club", "Select Group", choices = club_list),
  dataTableOutput("tab")
      )

server <- function(input, output, session) {
  seedval <- reactiveValues(
    row_priority = club_list,
    row_color = rep('white', 28)
  )
  
  observeEvent(input$club, {
    seedval$row_priority <- 
      c(input$club, seedval$row_priority[seedval$row_priority != input$club])
    seedval$row_color <- c('lightgreen', 'white', 'white')
  })
  
  output$tab <- renderDataTable(
    datatable(dat) %>% 
      formatStyle("club",
                  target = "row",
                  backgroundColor = styleEqual(seedval$row_priority, 
                                               seedval$row_color, 
                                               default = 'white')
      )
  )
}
shinyApp(ui, server)