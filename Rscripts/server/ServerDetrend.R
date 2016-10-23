# Plot
.GenerateDetrendPlot <- function() {
  if( !is.null(input$DetrendPlotSelectTraces) )
  {  
     r = .gtdetrend_plot(.GlobalEnv$tdd ,as.numeric( isolate(input$DetrendPlotSelectTraces) ) )
     if(is.character(r)) { # error case
       callModule( .ErrorMessage, id="DetrendErrorMessage", 
                   title = "Detrend plot:", 
                   errormessage = r, 
                   idcopy="DetrendErrorMessage")
     }
  }
}
output$DetrendPlot <- renderPlot({
    if(input$DetrendApplyBtn == 0)  plot.new()
    else  if(.GlobalEnv$.ErrorStatus$Detrend == FALSE)  .GenerateDetrendPlot()
})

# Plot options
output$DetrendPlotOptions <- renderUI ({
  if(input$DetrendApplyBtn>0 & (.GlobalEnv$.ErrorStatus$Detrend == FALSE ) )
  {
    choices <- select_traces_shiny( .GlobalEnv$td$query )
    fluidRow( 
      column( width=3,helpText(strong("Select traces to plot: ")) ),
      column( width=9,checkboxGroupInput(inline=TRUE,
                                         inputId="DetrendPlotSelectTraces",label=NULL,
                                         choices=choices, selected=choices) )
    )
  } # if end
}) 

# Help
observeEvent( input$DetrendHelpBtn, {
  callModule( .HelpPage,id="DetrendHelpPage", 
              helphtmlpage = .HelpHtmlFileName["Detrend"], 
              helpcss=.HelpCSSFileName, 
              idcopy="DetrendHelpPage")
})

# Apply
observeEvent( input$DetrendApplyBtn, priority = 1, {
# priority = 1 to check if .GlobalEnv$td exist and 
# to set .GlobalEnv$.ErrorStatus$Detrend <- TRUE for other observers
  if ( is.null(names(.GlobalEnv$td)) ) { # no google data yet
    .GlobalEnv$.ErrorStatus$Detrend <- TRUE
    callModule( .ErrorMessage, id="DetrendErrorMessage", 
                title = "Detrend:", 
                errormessage = "google data are not available", 
                idcopy="DetrendErrorMessage")
    return()
  }
  #------Progress bar start---------
  withProgress(
    message = 'Detrending data', 
    detail = "...",
    value = NULL, # hide progress indicator
    { 
      r <- .gtdetrend( .GlobalEnv$td, 
                      isolate(input$DetrendMethodSelectRBtn) )
    }) #------Progress bar end--------
  if( is.character(r) ) { # error case
    .GlobalEnv$.ErrorStatus$Detrend <- TRUE
    callModule( .ErrorMessage, id="DetrendErrorMessage", 
                title = "Detrend:", 
                errormessage = r, 
                idcopy="DetrendErrorMessage")
  }
  else { .GlobalEnv$tdd <- r # no error
    save(tdd, file=.tddFile )
    .GlobalEnv$.ErrorStatus$Detrend <- FALSE
    updateModelDataSelect(isolate(input$DetrendMethodSelectRBtn))
    updateCompareDataSelect( update = 'Detrend', 
                             detrend_method = isolate(input$DetrendMethodSelectRBtn))
    updateViewDataSelect   ( update = 'Detrend',
                             detrend_method = isolate(input$DetrendMethodSelectRBtn))
  }
})  

updateModelDataSelect <- function(detrend_method)
{
  .GlobalEnv$.modeldatachoices$possible$flag["Remainder components (seasonal decomposition)"] = 0
  .GlobalEnv$.modeldatachoices$possible$flag["Differenced data"] = 0
  .GlobalEnv$.modeldatachoices$possible$flag["Remainder components (smoothing)"] = 0

  switch( detrend_method,
    "season" = { .GlobalEnv$.modeldatachoices$possible$flag["Remainder components (seasonal decomposition)"] = 1 },
    "diff"   = { .GlobalEnv$.modeldatachoices$possible$flag["Differenced data"] = 1 },
    "smooth" = { .GlobalEnv$.modeldatachoices$possible$flag["Remainder components (smoothing)"] = 1 }
  )

  # update .modeldatachoices$actual 
  .GlobalEnv$.modeldatachoices$actual = c("Original traces" = "original") 
  for(i in 2:length(.GlobalEnv$.modeldatachoices$possible$flag))
    if(.GlobalEnv$.modeldatachoices$possible$flag[i] )  
       .GlobalEnv$.modeldatachoices$actual = c(.GlobalEnv$.modeldatachoices$actual, .GlobalEnv$.modeldatachoices$possible$value[i])

  updateRadioButtons( session,"ModelDataSelectRBtn", label=NULL,
                      choices = .GlobalEnv$.modeldatachoices$actual,
                      selected = "original", inline = FALSE )
}

updateCompareDataSelect <- function(update, detrend_method)
{
  if(update == 'Model') 
    .GlobalEnv$.comparedatachoices$possible$flag["Model Residuals"] = 1

  if(update == 'Detrend') {
    .GlobalEnv$.comparedatachoices$possible$flag["Seasonal components (seasonal decomposition)"] = 0
    .GlobalEnv$.comparedatachoices$possible$flag["Trend components (seasonal decomposition)"] = 0
    .GlobalEnv$.comparedatachoices$possible$flag["Remainder components (seasonal decomposition)"] = 0
    .GlobalEnv$.comparedatachoices$possible$flag["Differenced data"] = 0
    .GlobalEnv$.comparedatachoices$possible$flag["Smoothed data"] = 0
    .GlobalEnv$.comparedatachoices$possible$flag["Remainder components (smoothing)"] = 0

    switch( detrend_method,
            "season" = { .GlobalEnv$.comparedatachoices$possible$flag["Seasonal components (seasonal decomposition)"] = 1
                         .GlobalEnv$.comparedatachoices$possible$flag["Trend components (seasonal decomposition)"] = 1
                         .GlobalEnv$.comparedatachoices$possible$flag["Remainder components (seasonal decomposition)"] = 1 },
            "diff"   = { .GlobalEnv$.comparedatachoices$possible$flag["Differenced data"] = 1 },
            "smooth" = { .GlobalEnv$.comparedatachoices$possible$flag["Smoothed data"] = 1 
                         .GlobalEnv$.comparedatachoices$possible$flag["Remainder components (smoothing)"] = 1 }
    ) 
  }

  # update .comparedatachoices$actual
  .GlobalEnv$.comparedatachoices$actual = c("Original traces" = "original") 
  for(i in 2:length(.GlobalEnv$.comparedatachoices$possible$flag))
    if(.GlobalEnv$.comparedatachoices$possible$flag[i] )  
       .GlobalEnv$.comparedatachoices$actual = c(.GlobalEnv$.comparedatachoices$actual, .GlobalEnv$.comparedatachoices$possible$value[i])

  updateRadioButtons( session,"CompareDataSelectRBtn", label=NULL,
                      choices = .GlobalEnv$.comparedatachoices$actual,
                      selected = "original", inline = FALSE )
}

updateViewDataSelect <- function(update, detrend_method, compare_method)
{
  if(update == 'Model') {
    .GlobalEnv$.viewdatachoices$possible$flag["Model Residuals"] = 1
    .GlobalEnv$.viewdatachoices$possible$flag["Model Parameters"] = 1
    .GlobalEnv$.viewdatachoices$possible$flag["Model LB Test"] = 1
  }

  if(update == 'Detrend') {
    .GlobalEnv$.viewdatachoices$possible$flag["Seasonal components (seasonal decomposition)"] = 0
    .GlobalEnv$.viewdatachoices$possible$flag["Trend components (seasonal decomposition)"] = 0
    .GlobalEnv$.viewdatachoices$possible$flag["Remainder components (seasonal decomposition)"] = 0
    .GlobalEnv$.viewdatachoices$possible$flag["Differenced data"] = 0
    .GlobalEnv$.viewdatachoices$possible$flag["Smoothed data"] = 0
    .GlobalEnv$.viewdatachoices$possible$flag["Remainder components (smoothing)"] = 0

    switch( detrend_method,
            "season" = { .GlobalEnv$.viewdatachoices$possible$flag["Seasonal components (seasonal decomposition)"] = 1
                         .GlobalEnv$.viewdatachoices$possible$flag["Trend components (seasonal decomposition)"] = 1
                         .GlobalEnv$.viewdatachoices$possible$flag["Remainder components (seasonal decomposition)"] = 1 },
            "diff"   = { .GlobalEnv$.viewdatachoices$possible$flag["Differenced data"] = 1 },
            "smooth" = { .GlobalEnv$.viewdatachoices$possible$flag["Smoothed data"] = 1 
                         .GlobalEnv$.viewdatachoices$possible$flag["Remainder components (smoothing)"] = 1 }
    ) 
  }
  
  if(update == 'Compare') {
    .GlobalEnv$.viewdatachoices$possible$flag["Autocorr. sgn."] = 0
    .GlobalEnv$.viewdatachoices$possible$flag["Crosscorr. lag0"] = 0
    .GlobalEnv$.viewdatachoices$possible$flag["Crosscorr. max."] = 0
    .GlobalEnv$.viewdatachoices$possible$flag["Crosscorr. max. lag"] = 0
    .GlobalEnv$.viewdatachoices$possible$flag["Synchrony"] = 0

    switch( compare_method,
            "autocorr"   = {  
              .GlobalEnv$.viewdatachoices$possible$flag["Autocorr. sgn."] = 1
            },
            "crosscorr"  = {  
              .GlobalEnv$.viewdatachoices$possible$flag["Crosscorr. lag0"] = 1
              .GlobalEnv$.viewdatachoices$possible$flag["Crosscorr. max."] = 1
              .GlobalEnv$.viewdatachoices$possible$flag["Crosscorr. max. lag"] = 1
            },
            "synchrony"  = {  
              .GlobalEnv$.viewdatachoices$possible$flag["Synchrony"] = 1
            }
    ) 
  }
  # update .viewdatachoices$actual
  .GlobalEnv$.viewdatachoices$actual = c("Original traces" = "original") 
  for(i in 2:length(.GlobalEnv$.viewdatachoices$possible$flag))
    if(.GlobalEnv$.viewdatachoices$possible$flag[i] )  
       .GlobalEnv$.viewdatachoices$actual = c(.GlobalEnv$.viewdatachoices$actual, .GlobalEnv$.viewdatachoices$possible$value[i])
     
  updateRadioButtons( session,"DataSelectRBtn", label=NULL,
                      choices = .GlobalEnv$.viewdatachoices$actual,
                      selected = "original", inline = FALSE )
}

