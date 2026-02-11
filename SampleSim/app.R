source('function-definitions.R')

library(shiny)

ui <- fluidPage(

    titlePanel('Survey Simulator'),

    sidebarLayout(
        sidebarPanel(
            # Input population size
            sliderInput('pop.n',
                        'Population size:',
                        min = 0,
                        max = 1e6,
                        value = 1e4),
            # Input population agreement
            sliderInput('pop.p',
                        'Population proportion:',
                        min = 0,
                        max = 100,
                        value = 50,
                        post = ' %'),
            # Input sample size
            sliderInput('sample.n',
                        'Sample size:',
                        min = 0,
                        max = 5e3,
                        value = 1e3)
            ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput('distPlot')
        )
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

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
shinyApp(ui = ui, server = server)
