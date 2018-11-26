library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Visualization"),
  dashboardSidebar(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          box(selectInput(inputId = "ID_number"),
              label = "Select ID:",
              choices = c("a", "b")))
        )  
      )
    )
  ),
  dashboardBody(
    mainPanel(
      box(plotOutput("main_plot")),
      box(plotOutput("sub_plot"))
      box(plotOutput("table"))
    )
  )
)

server <- function(input, output){
  # return the requested chart
  id_list
  req_id <- reactive({input$ID_number %in% id_list})
  # create chart list object
  chart <- 
  output$main_plot <- renderPlot({
    chart[[1]]
  })
  output$subplot <- renderPlot({chart[[2]]})
  output$table <- renderTable(chart[[""]])
}

shiny(ui, server)
