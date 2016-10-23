# Plot
.GenerateComparePlot <- function() {
  
  if( is.null(input$ComparePlotSelectTraces) ) selectedtraces = c(-1)
  else selectedtraces = as.numeric( isolate(input$ComparePlotSelectTraces) )

  if( is.null(input$ComparePlotSelectACFRBtn) ) selectedacf = -1
  else selectedacf = as.numeric( isolate(input$ComparePlotSelectACFRBtn) )
  
  if( is.null(input$ComparePlotSelectCCF) ) selectedccf = c(-1)
  else selectedccf = as.numeric( isolate(input$ComparePlotSelectCCF) )
  
  if( is.null(input$ComparePlotSelectCmRBtn) ) selectedcm = -1
  else selectedcm = as.numeric( isolate(input$ComparePlotSelectCmRBtn) )
  
  r = .gtcompare_plot(.GlobalEnv$tc, selectedtraces, selectedacf, selectedccf, selectedcm )             
  if(is.character(r)) { # error case
    callModule( .ErrorMessage, id="CompareErrorMessage", 
                title = "Compare plot:", 
                errormessage = r, 
                idcopy="CompareErrorMessage")
  }
}

output$ComparePlot <- renderPlot({
  if(input$CompareApplyBtn == 0)  plot.new()
  else  if(.GlobalEnv$.ErrorStatus$Compare == FALSE)  .GenerateComparePlot()
})

# Plot options
output$ComparePlotOptions <- renderUI ({
  if(input$CompareApplyBtn>0 & (.GlobalEnv$.ErrorStatus$Compare == FALSE ) )
  {
    choices <- select_traces_shiny( .GlobalEnv$td$query )
    switch ( isolate(input$CompareMethodSelectRBtn), 
      "autocorr" = { 
          fluidRow( 
             column( width=3, helpText( strong("Select traces to plot: ") ) ),
             column( width=9, checkboxGroupInput(inline=TRUE, inputId="ComparePlotSelectTraces",
                                choices = choices, selected = choices, label=NULL ) ),
             column( width=3, helpText(strong("Autocorrelation to plot: ")) ),
             column( width=9, radioButtons( inline = TRUE, inputId = "ComparePlotSelectACFRBtn",
                                choices = choices,  label = NULL ) )
          )
      },
      "crosscorr" = { 
        fluidRow( 
          column( width=3, helpText( strong("Select traces to plot: ") ) ),
          column( width=9, checkboxGroupInput(inline = TRUE, inputId="ComparePlotSelectTraces",
                                choices = choices, selected = choices, label=NULL ) ),
          column( width=3, helpText( strong("Crosscorrelation to plot: ")) ),
          column( width=9, checkboxGroupInput(inline = TRUE, inputId = "ComparePlotSelectCCF",
                                choices = choices, selected = c(1,2), label = NULL ) ),
          column( width=3, helpText( strong("Crosscorrelation martix to plot: ")) ),
          column( width=9, radioButtons( inline = TRUE, inputId = "ComparePlotSelectCmRBtn", label = NULL,
                                choices = c( "Crosscorr. at lag0" = 1, "Crosscorr. maximum" = 2, "Lag for maximum crosscorr." = 3 ) ) )
        )
      },
      "synchrony" = { 
          fluidRow( 
            column( width=3, helpText( strong("Select traces to plot: ") ) ),
            column( width=9, checkboxGroupInput(inline=TRUE, inputId="ComparePlotSelectTraces", label=NULL,
                                             choices = choices, selected = choices ) )
          ) 
      }  
    ) # switch end
  } # if end
}) 

# Help
observeEvent( input$CompareHelpBtn, {
  callModule( .HelpPage,id="CompareHelpPage", 
              helphtmlpage = .HelpHtmlFileName["Compare"], 
              helpcss=.HelpCSSFileName, 
              idcopy="CompareHelpPage")
})

# Apply
observeEvent( input$CompareApplyBtn, priority = 1, {
  # priority = 1 to set .GlobalEnv$.ErrorStatus$Compare <- TRUE for other observers
  #------Progress bar start---------
  withProgress(
    message = 'Comparing data', 
    detail = "...",
    value = NULL, # hide progress indicator
    { 
      r <- .gtcompare( selecteddata = isolate(input$CompareDataSelectRBtn),
                      method = isolate(input$CompareMethodSelectRBtn),
                      options = list("cbands"=isolate(input$CompareCBandsSelect))   )
    }  
  ) #------Progress bar end--------
  if( is.character(r) ) { # error case
    .GlobalEnv$.ErrorStatus$Compare <- TRUE
    callModule( .ErrorMessage, id="CompareErrorMessage", 
                title = "Compare:", 
                errormessage = r, 
                idcopy="CompareErrorMessage")
  }
  else { .GlobalEnv$tc <- r # no error
    save(tc, file=.tcFile )
    .GlobalEnv$.ErrorStatus$Compare <- FALSE
     updateViewDataSelect   ( update = 'Compare', 
                              compare_method = isolate(input$CompareMethodSelectRBtn))
  }
  
})
