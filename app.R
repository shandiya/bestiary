library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Eagle+Lake&display=swap');
      .center-row {
        display: flex;
        justify-content: center;
        margin: 25px 0;
      }
      .center-screen {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100vh;
      }
      #getBeastieFirst, #getBeastie, #keepBeastie {
        border: none;
        outline: none;
        font-size: 1.2em;
        font-family: 'Eagle Lake', serif;
      }"))
  ),
  
  # to make button appear in center screen at first
  conditionalPanel(
    condition = "input.getBeastieFirst == 0",
    div(class = "center-screen",
        actionButton("getBeastieFirst", "Show me a wee beastie"))
  ),
  
  conditionalPanel(
    condition = "input.getBeastieFirst > 0",
    div(class = "center-row",
        actionButton("getBeastie", "Show me a wee beastie"))
  ),
  
  plotOutput("outputPlot"),
  
  conditionalPanel(
    condition = "input.getBeastieFirst > 0",
    div(class = "center-row",
        downloadButton("keepBeastie", "Adopt this beastie!"))
  )
)
  
server <- function(input, output, session) {
  
  beastie <- eventReactive(c(input$getBeastieFirst, input$getBeastie), {
    
    full_grid <- expand.grid(x = 1:10, y = 1:10)
    n_points <- sample(6:15, 1)
    rows <- sample(1:nrow(full_grid), n_points, replace = FALSE)
    subset_grid <- full_grid[rows, ]
    
    ggplot() +
      geom_point(data = rbind(head(subset_grid, 1),
                              tail(subset_grid, 1)),
                 aes(x, y),
                 size = 0.7) +
      geom_path(data = subset_grid,
                aes(x, y)) +
      coord_equal() +
      theme_void()
  }, ignoreInit = TRUE)
  
  output$outputPlot <- renderPlot({
    beastie()
  })
  
  output$keepBeastie <- downloadHandler(
    filename = function() {
      paste0("beastie_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      ggsave(file, plot = beastie(), device = "png",
             width = 6, height = 6, dpi = 300)
    }
  )
}
    
shinyApp(ui = ui, server = server)
