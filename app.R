source('function-definitions.R')

library(shiny)

ui <- fluidPage(
  
  titlePanel('Sampling Simulator'),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('population.N',
                  'Population size:',
                  min = 1e4,
                  max = 1e5,
                  value = 1e4,
                  step = 1000),
      sliderInput('population.p',
                  'Population proportion:',
                  min = 0,
                  max = 100,
                  value = 50,
                  step = 2.5,
                  post = ' %'),
      sliderInput('sample.n',
                  'Sample size:',
                  min = 50,
                  max = 5e3,
                  value = 50,
                  step = 25),
      sliderInput('ddc',
                  'Data defect correlation:',
                  min = -.25,
                  max = .25,
                  value = 0,
                  step = .05),
      checkboxInput('include.mean', 'Mean of samples', FALSE),
      radioButtons(
        'range.function',
        'Range function',
        choices = list('None', 'Minimum-maximum', 'Standard deviation')
      ),
      radioButtons(
        'confint',
        'Confidence interval',
        choices = list(None = 'None', '90%' = '.90', '95%' = '.95', '99%' = '.99')
      ),
      actionButton(
        'collect',
        'Collect data from 10 samples'
      )
    ),
    # Plot the results
    mainPanel(
      plotOutput('samplePlot')
    )
  )
)

server <- function(input, output) {
  
  output$samplePlot <- renderPlot({
    
    true.p <- input$population.p / 100
    
    if(input$range.function == 'None') {
      range.function = NULL
    } else if(input$range.function == 'Minimum-maximum') {
      range.function = minmax
    } else if(input$range.function == 'Standard deviation') {
      range.function = sdrange
    }
    
    if(input$confint == 'None') {
      confint <- NULL
    } else {
      confint <- as.numeric(input$confint)
    }
    
    population <- c(
      rep(1, input$population.N * true.p), 
      rep(0, input$population.N * (1 - true.p)))
    
    random.samples <- eventReactive(
      input$collect,
      {
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
        )
      }
    )
    
    random.samples() %>%
      visualise.samples(
        include.mean = input$include.mean,
        range.function = range.function,
        confint = confint
      )
    
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
