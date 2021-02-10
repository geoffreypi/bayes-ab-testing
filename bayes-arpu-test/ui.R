# bayes-arpu-test

library(shiny)
library(plotly)

n = 2

input_controls = selectInput('n','Number of Groups',(1:10))
input_names = ""
group_names = ""

if (n>0) {
  for(i in 0:(n-1)) {
    # get letter A,B,C,etc
    c = intToUtf8(65+i)
    
    s = paste0('rev_',c)
    input_names = c(input_names,s)
    # add an input for total revenue
    input_controls <- tagAppendChild(input_controls, numericInput(
      s,
      paste0('total revenue ',c,':'),
      min = 0,
      max = 1e6,
      value = 5000
    ))
    s = paste0('success_',c)
    input_names = c(input_names,s)
    # add an input for people who converted (payed)
    input_controls <- tagAppendChild(input_controls, numericInput(
      s,
      paste0('converted payers ',c,':'),
      min = 0,
      max = 1e6,
      value = 200
    ))
    s = paste0('total_',c)
    input_names = c(input_names,s)
    # add an input for the total number of players
    input_controls <- tagAppendChild(input_controls, numericInput(
      s,
      paste0('all players ',c,':'),
      min = 0,
      max = 1e6,
      value = 10000
    ))
    # call the first group control, all the others test 1, test 2, etc
    if (i==0) {
      group_names[i+1] = 'control'
    } else {
      group_names[i+1] = paste0('test ',i)
    }
    s = paste0('name_',c)
    input_names = c(input_names,s)
    # add an input to name each group
    input_controls <- tagAppendChild(input_controls, textInput(
      s,
      paste0('name ',c,':'),
      value = group_names[i+1]
    ))
  }
}

input_names = input_names[-(1:1)]

print(input_names)

print(input_controls)

shinyUI(
  fluidPage(
    titlePanel('Bayesian A/B/n test for ARPU'),
    sidebarLayout(
      sidebarPanel(
        input_controls,
        selectInput('group1name','First Group',group_names),
        selectInput('group2name','Second Group',group_names),
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