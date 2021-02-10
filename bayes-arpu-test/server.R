# bayes-arpu-test

library(shiny)
library(plotly)
source('utilities.R')

shinyServer(function(input, output) {
  
  # Initialize list of inputs
  inputTagList <- tagList()
  
  output$allInputs <- renderUI({
    n <- input$n_groups
    
    g_list = LETTERS[1:n]
    inputTagList <- tagAppendChild(inputTagList, selectInput('group1idx','First Group',g_list))
    inputTagList <- tagAppendChild(inputTagList, selectInput('group2idx','Second Group',g_list))
    group_names = "control"
    for(i in 1:n) {
      # get letter A,B,C,etc
      c = intToUtf8(64+i)
      
      # add an input for total revenue
      inputTagList <- tagAppendChild(inputTagList, numericInput(
        paste0('rev_',c),
        paste0('total revenue ',c,':'),
        min = 0,
        max = 1e6,
        value = 5000
      ))
      # add an input for people who converted (payed)
      inputTagList <- tagAppendChild(inputTagList, numericInput(
        paste0('success_',c),
        paste0('converted payers ',c,':'),
        min = 0,
        max = 1e6,
        value = 200
      ))
      # add an input for the total number of players
      inputTagList <- tagAppendChild(inputTagList, numericInput(
        paste0('total_',c),
        paste0('all players ',c,':'),
        min = 0,
        max = 1e6,
        value = 10000
      ))
      # call the first group control A, all the others test B, test C, etc
      if (i>1) {
        group_names[i] = paste0('test ',c)
      }
      # add an input to name each group
      inputTagList <- tagAppendChild(inputTagList, textInput(
        paste0('name_',c),
        paste0('name ',c,':'),
        value = group_names[i]
      ))
    }
    inputTagList
  })
  
  observeEvent(input$button, {
    rev_A <- input[[paste0('rev_',input$group1idx)]]
    rev_B <- input[[paste0('rev_',input$group2idx)]]
    success_A <- input[[paste0('success_',input$group1idx)]]
    success_B <- input[[paste0('success_',input$group2idx)]]
    total_A <- input[[paste0('total_',input$group1idx)]]
    total_B <- input[[paste0('total_',input$group2idx)]]
    name_A <- input[[paste0('name_',input$group1idx)]]
    name_B <- input[[paste0('name_',input$group2idx)]]
    
    print(success_A)
    if(
      success_A >= 0 &&
      total_A > 0 &&
      success_B >= 0 &&
      total_B > 0 &&
      success_A <= total_A &&
      success_B <= total_B &&
      rev_A >= 0 &&
      rev_B >= 0 &&
      input$sim_sample >= 2
    ) {
      sample_A <- isolate({total_A})
      sample_B <- isolate({total_B})
      conv_A <- isolate({success_A/total_A})
      conv_B <- isolate({success_B/total_B})
      arppu_A <- isolate({rev_A/success_A})
      arppu_B <- isolate({rev_B/success_B})
      arpu_A <- isolate({rev_A/total_A})
      arpu_B <- isolate({rev_B/total_B})
      alpha_A <- isolate({success_A + 1})
      alpha_B <- isolate({success_B + 1})
      beta_A <- isolate({total_A - success_A + 1})
      beta_B <- isolate({total_B - success_B + 1})
      k_A <- isolate({success_A + 1})
      k_B <- isolate({success_B + 1})
      theta_A <- isolate({1/(1 + rev_A)})
      theta_B <- isolate({1/(1 + rev_B)})
      res <- isolate({
        bayes_arpu(
          alphaA = alpha_A, betaA = beta_A,
          kA = k_A, thetaA = theta_A,
          alphaB = alpha_B, betaB = beta_B,
          kB = k_B, thetaB = theta_B,
          MSamples = input$sim_sample
        )
      })
      post_sample_A <- res$sampleLambdaA/res$sampleOmegaA
      post_sample_B <- res$sampleLambdaB/res$sampleOmegaB
      diff_post_sample <- post_sample_B - post_sample_A
      hdi_A <- hdi_of_sample(post_sample_A)
      hdi_B <- hdi_of_sample(post_sample_B)
      hdi_diff <- hdi_of_sample(diff_post_sample)
      x_lim <- {
        a <- min(hdi_A, hdi_B)
        b <- max(hdi_A, hdi_B)
        c(1.2*a - 0.2*b, 1.2*b - 0.2*a)
      }
      x_lim_diff <- {
        a <- hdi_diff[1]
        b <- hdi_diff[2]
        c(1.2*a - 0.2*b, 1.2*b - 0.2*a)
      }
      name_A <- isolate({name_A})
      name_B <- isolate({name_B})
      printPlot <- isolate({TRUE})
    } else {
      sample_A <- isolate({0})
      sample_B <- isolate({0})
      conv_A <- isolate({NaN})
      conv_B <- isolate({NaN})
      arppu_A <- isolate({NaN})
      arppu_B <- isolate({NaN})
      arpu_A <- isolate({NaN})
      arpu_B <- isolate({NaN})
      alpha_A <- isolate({1})
      alpha_B <- isolate({1})
      beta_A <- isolate({1})
      beta_B <- isolate({1})
      k_A <- isolate({1})
      k_B <- isolate({1})
      theta_A <- isolate({1})
      theta_B <- isolate({1})
      hdi_A <- isolate({c(NaN, NaN)})
      hdi_B <- isolate({c(NaN, NaN)})
      hdi_diff <- isolate({c(NaN, NaN)})
      x_lim <- isolate({c(NaN, NaN)})
      x_lim_diff <- isolate({c(NaN, NaN)})
      res <- isolate({
        list(
          convProbBbeatsA = NaN, convExpLossA = NaN, convExpLossB = NaN,
          revProbBbeatsA = NaN, revExpLossA = NaN, revExpLossB = NaN,
          arpuProbBbeatsA = NaN, arpuExpLossA = NaN, arpuExpLossB = NaN
        )
      })
      name_A <- isolate({''})
      name_B <- isolate({''})
      printPlot <- isolate({FALSE})
    }
    output$table1 <- renderTable({
      tab <- data.frame(
        metric = c(
          'sample size', 'conversion', 'ARPPU',
          '<strong>ARPU<strong>', '<strong>95% HDI<strong>'
        ),
        A = c(
          sprintf('\n%.d', sample_A),
          sprintf('\n%.3g%%', conv_A*100),
          sprintf('\n%.4g €', arppu_A),
          sprintf('\n%.4g €', arpu_A),
          sprintf('[ %.3g €, \n%.3g € ]', hdi_A[1], hdi_A[2])
        ),
        B = c(
          sprintf('\n%.d', sample_B),
          sprintf('\n%.3g%%', conv_B*100),
          sprintf('\n%.4g €', arppu_B),
          sprintf('\n%.4g €', arpu_B),
          sprintf('[ %.3g €, \n%.3g € ]', hdi_B[1], hdi_B[2])
        ),
        BA = c(
          sprintf(' '),
          sprintf('\n%.3g%%', conv_B*100 - conv_A*100),
          sprintf('\n%.4g €', arppu_B - arppu_A),
          sprintf('\n%.4g €', arpu_B - arpu_A),
          sprintf('[ %.3g €, \n%.3g € ]', hdi_diff[1], hdi_diff[2])
        )
      )
      colnames(tab) <- c(
        ' ',
        paste('A -', name_A),
        paste('B -', name_B),
        'B - A'
      )
      tab
    }, spacing = 'xs', sanitize.text.function = function(x){x})
    output$table2 <- renderTable({
      tab <- data.frame(
        column1 = c(
          'Probability that B is better than A',
          'Expected uplift if B is actually better',
          'Expected loss if B is actually worse'
        ),
        conversion = c(
          sprintf('\n%.1f%%', res$convProbBbeatsA*100),
          sprintf('\n%.2g%%', res$convExpLossA*100),
          sprintf('\n%.2g%%', res$convExpLossB*100)
        ),
        ARPPU = c(
          sprintf('\n%.1f%%', res$revProbBbeatsA*100),
          sprintf('\n%.2g €', res$revExpLossA),
          sprintf('\n%.2g €', res$revExpLossB)
        ),
        ARPU = c(
          sprintf('\n%.1f%%', res$arpuProbBbeatsA*100),
          sprintf('\n%.2g €', res$arpuExpLossA),
          sprintf('\n%.2g €', res$arpuExpLossB)
        )
      )
      colnames(tab) <- c(' ', 'conversion', 'ARPPU', 'ARPU')
      tab
    }, spacing = 'xs')
    output$posterior_plot_B_minus_A <- renderPlotly({
      plot_sample_density(
        sample = diff_post_sample,
        hdi = hdi_diff,
        printPlot = printPlot
      )
    })
    output$posterior_plot <- renderPlotly({
      plot_sample_densities(
        sampleDataA = post_sample_A,
        sampleDataB = post_sample_B,
        hdiA = hdi_A,
        hdiB = hdi_B,
        paste('', name_A),
        paste('', name_B),
        printPlot = printPlot
      )
    })
  })
  
})