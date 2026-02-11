source('function-definitions.R')

library(shiny)

ui <- fluidPage(

    titlePanel('Survey Simulator'),

    sidebarLayout(
        sidebarPanel(
            # Input population size
            sliderInput('population.N',
                        'Population size:',
                        min = 1e4,
                        max = 1e5,
                        value = 1e4),
            # Input population agreement
            sliderInput('population.p',
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
                        value = 0),
            # Input data defect correlation
            sliderInput('ddc',
                        'Data defect correlation:',
                        min = -.5,
                        max = .5,
                        value = 0,
                        step = .05),
            # 
            checkboxInput('population.line', 'Population comparison', FALSE),
            checkboxInput('include.mean', 'Mean of samples', FALSE),
            radioButtons(
              'confint',
              'Confidence interval for samples',
              choices = list('None', '.90', '.95', '.99')
            )
            ),
        # Plot the results
        mainPanel(
          plotOutput('samplePlot')
        )
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$samplePlot <- renderPlot({
      
      true.p <- input$population.p / 100
      if(input$confint == 'None') {
        confint <- NULL
      } else {
        confint <- as.numeric(input$confint)
      }
      
      population <- c(
        rep(1, input$population.N * true.p), 
        rep(0, input$population.N * (1 - true.p)))
      
      if(input$sample.n > 0) {
        samples <- (1:10) %>%
          map_dbl(
            function(x) {
              mean(
                take.sample(
                  population, 
                  input$sample.n, 
                  data.defect.correlation = input$ddc))
            }
          )
        
        tibble(
          Sample = c('Pop.', 1:length(samples)), 
          n = c(NA, rep(input$sample.n, length(samples))),
          p = c(mean(population), samples)
        ) %>%
          visualise.samples(
            population.line = input$population.line,
            include.mean = input$include.mean,
            confint = confint
          )
        } else {
          tibble(
            Sample = c('Pop.', 1:10), 
            n = NA, 
            p = c(mean(population), rep(NA, 10))
          ) %>% 
            visualise.samples(
              population.line = input$population.line
            )
      }
        
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
