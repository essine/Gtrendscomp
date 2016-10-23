# Plot
.GenerateModelPlot <- function() {
  
  if( is.null(input$ModelPlotSelectTraces) ) selectedtraces = c(-1)
  else selectedtraces = as.numeric( isolate(input$ModelPlotSelectTraces) )
                                    
  if( is.null(input$ModelPlotSelectCoeff)  ) return() 
  else selectedcoeff = as.numeric( isolate(input$ModelPlotSelectCoeff) )
                                    
  r = .gtmodel_plot(.GlobalEnv$tm , selectedtraces, selectedcoeff )
  if(is.character(r)) { # error case
       callModule( .ErrorMessage, id="ModelErrorMessage", 
                   title = "Model plot:", 
                   errormessage = r, 
                   idcopy="ModelErrorMessage")
  }
}
                   
output$ModelPlot <- renderPlot({
    if(input$ModelApplyBtn == 0)  plot.new()
    else  if(.GlobalEnv$.ErrorStatus$Model == FALSE)  .GenerateModelPlot()
})

# Plot options
output$ModelPlotOptions <- renderUI ({
  if(input$ModelApplyBtn>0 & (.GlobalEnv$.ErrorStatus$Model == FALSE ) )
  {
    choices <- select_traces_shiny( .GlobalEnv$td$query )
    fluidRow( 
      column( width=3,helpText(strong("Select traces to plot: ")) ),
      column( width=9,checkboxGroupInput(inline=TRUE, inputId="ModelPlotSelectTraces", label=NULL,
                                         choices = choices, selected = choices) ),
      column( width=3,helpText(strong("Model coefficients to plot: ")) ),
      column( width=9,radioButtons( inline = TRUE, inputId = "ModelPlotSelectCoeff", label = NULL, 
                                    choices = choices, selected = choices[1]) )      
    ) # fluidRow end
  } # if end
}) 

# Help
observeEvent( input$ModelHelpBtn, {
  callModule( .HelpPage,id="ModelHelpPage", 
              helphtmlpage = .HelpHtmlFileName["Model"], 
              helpcss=.HelpCSSFileName, 
              idcopy="ModelHelpPage")
})

# Apply
observeEvent( input$ModelApplyBtn, priority = 1, {
# priority = 1 to set .GlobalEnv$.ErrorStatus$Model <- TRUE for other observers
  if ( is.null( names(.GlobalEnv$tdd) ) ) { # no detrended data yet
    if( is.null( names(.GlobalEnv$td)  ) ) { # no google data yet
      .GlobalEnv$.ErrorStatus$Model <- TRUE
      callModule( .ErrorMessage, id="ModelErrorMessage", 
                  title = "Model:", 
                  errormessage = "data are not available",
                  idcopy="ModelErrorMessage")
      return()
    }
    else data_to_model <- .GlobalEnv$td
  }
  else data_to_model <- .GlobalEnv$tdd
  #------Progress bar start---------
  withProgress(
    message = 'Modeling data', 
    detail = "...",
    value = NULL, # hide progress indicator
    { 
      r <- .gtmodel( inputdata  = data_to_model, 
                    selectdata = isolate(input$ModelDataSelectRBtn) )
    }  
  ) #------Progress bar end--------
  if( is.character(r) ) { # error case
    .GlobalEnv$.ErrorStatus$Model <- TRUE
    callModule( .ErrorMessage, id="ModelErrorMessage", 
                title = "Model:", 
                errormessage = r, 
                idcopy="ModelErrorMessage")
  }
  else { .GlobalEnv$tm <- r # no error
    save(tm, file=.tmFile )
    .GlobalEnv$.ErrorStatus$Model <- FALSE
    updateCompareDataSelect(update = 'Model')
    updateViewDataSelect( update = 'Model')
  }
})  
