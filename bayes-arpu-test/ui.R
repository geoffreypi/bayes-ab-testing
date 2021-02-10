# bayes-arpu-test

library(shiny)
library(plotly)

shinyUI(
  fluidPage(
    titlePanel('Bayesian A/B/n test for ARPU'),
    sidebarLayout(
      sidebarPanel(
        selectInput('n_groups','Number of Groups',(2:10),5),
        numericInput(
          'sim_sample',
          'sample size used in simulations:',
          min = 2,
          max = 1e7,
          value = 1e5
        ),
        actionButton(
          'button',
          'Calculate'
        ),
        uiOutput("allInputs"),
        hr(),
        tags$div(
          class='header', checked = NA,
          # tags$a(
          #   href = 'http://confluence.pixelfederation.com/pages/viewpage.action?pageId=4607085',
          #   'Click here to read how to use this calculator'
          # ),
          tags$br(),
          tags$a(
            href = 'https://github.com/Vidogreg/bayes-ab-testing',
            'Get the code here'
          ),
          tags$p('support: vidogreg@gmail.com')
        )
      ),
      mainPanel(
        tableOutput('table1'),
        tableOutput('table2'),
        hr(),
        plotlyOutput('posterior_plot_B_minus_A', width = '580px', height = '250px'),
        hr(),
        plotlyOutput('posterior_plot', width = '580px', height = '250px')
      )
    )
  )
)