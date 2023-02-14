library(shiny)

# Define UI for application that draws a histogram
stipend_comparison_ui <- fluidPage(

    # Application title
    titlePanel("Comparison of Stipends by Program"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
stipend_comparison_server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 

runComparisonApp <- function() {
  ui <- fluidPage(
    stipend_comparison_server()
  )
  
  server <- function(input, output, session) {
    stipend_comparison_server(input, output)
  }
  shinyApp(ui, server)
}
