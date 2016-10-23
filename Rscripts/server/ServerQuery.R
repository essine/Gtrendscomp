# Plot
.GenerateQueryPlot <- function() {
   if(!is.null(input$QueryPlotSelectTraces) )
   {  
     r = .gtquery_plot(.GlobalEnv$td ,selected = as.numeric(isolate(input$QueryPlotSelectTraces)) )
     if(is.character(r)) { # error case
       callModule( .ErrorMessage, id="QueryErrorMessage", 
                   title = "qtquery_plot:", 
                   errormessage = r, 
                   idcopy="QueryErrorMessage")
     }
   }
}
output$QueryPlot <- renderPlot({
  if(input$QueryExample)  .GenerateQueryPlot()
  else
  { 
    if(input$QueryApplyBtn == 0)  plot.new()
    else  if(.GlobalEnv$.ErrorStatus$Query == FALSE) .GenerateQueryPlot()
  }
})

# Plot options
select_traces_shiny <- function(data)
{ 
  choices <- c(1:length(data))
  names(choices) <- data
  return(choices) 
}

output$QueryPlotOptions <- renderUI ({
  if(input$QueryExample | (input$QueryApplyBtn>0 & (.GlobalEnv$.ErrorStatus$Query == FALSE)) ) {
    
    choices <- select_traces_shiny( .GlobalEnv$td$query )
    
    fluidRow( 
      column( width=3,helpText(strong("Select traces to plot: ")) ),
      column( width=9,checkboxGroupInput(inline=TRUE,
                                         inputId="QueryPlotSelectTraces",label=NULL,
                                         choices=choices, selected=choices) )
    )
  }#if end
})  

# Example data
update_queries <- function(queries){
  k=1
  for(i in 1:5) { t = character(0); b = FALSE
    if( i <= length(queries) ) # query from "td" data
    { 
        t = queries[i]; b = TRUE; k = k+1
    }
    updateTextInput(session,inputId=.lsr$id[[i]],label=NULL,value=t)
    updateCheckboxInput(session,.lsr$boxid[[i]],label=NULL,value=b)
  }
}
  
observeEvent (input$QueryExample, priority = 1, {  # Generate example
  # priority = 1 to create in time .GlobalEnv$td$query for output$QueryPlotOptions
    if(input$QueryExample) { # checkbox "Example" ON
      load(.tdExampleFile); .GlobalEnv$td <- td
      updateTextInput(session,inputId="QueryStartDate",label=NULL,value=td$data$Date[1])
      updateTextInput(session,inputId="QueryEndDate",label=NULL,value=td$data$Date[length(td$data$Date)])
      updateSelectInput(session,inputId="QueryDataTypeSelect",label=NULL,choices=NULL,selected=td$datatype)

      updateSelectInput(session,inputId="QueryOptionScaleSelect",label=NULL, choices = NULL,selected=td$timescale )
      updateTextInput(session,  inputId="QueryOptionGeo",        label=NULL, value   = toString(td$geo) )
      updateTextInput(session,  inputId="QueryOptionCategory",   label=NULL, value   = toString(td$category) )
      
      uq <- update_queries(td$query)
    }
})


# Retrieve data from google and plot it

# prepaires "lsearch" input vector for ".gtquery()" function
select_queries <- function()
{
  srch <- c(input$QuerySw1, input$QuerySw2, input$QuerySw3, input$QuerySw4, input$QuerySw5)
  cbx  <- c(input$QueryCb1, input$QueryCb2, input$QueryCb3, input$QueryCb4, input$QueryCb5)
  queries = c(); k=0
  for(i in 1:length(srch))
  { if(cbx[i]) { # trend is selected for query
       k=k+1;  queries[k] <- srch[i]
    }
  }
  return(queries)
} 
 
observeEvent( input$QueryApplyBtn, priority = 1, {
# priority = 1 to create in time .GlobalEnv$td$query for output$QueryPlotOptions    
  
  #------Progress bar start---------
  withProgress(
     message = 'Retrieving data from google', 
     detail = "...",
     value = NULL, # hide progress indicator
     { # perform qtquery with progress message
       if(.GlobalEnv$.shinyappsio) 
         r <- "Query from 'Google Trends' is inactive at 'shinyapps.io'. Please install 'Gtrendscomp' locally to retrieve data from 'Google Trends' or upload CSV data. The example data can be also used."
       else
         r <- .gtquery(usr = isolate(input$QueryUser),
                      pwd = isolate(input$QueryPassword),
                      lsearch = select_queries(),
                      daterange = c( isolate(input$QueryStartDate), isolate(input$QueryEndDate) ),
                      timescale = isolate(input$QueryOptionScaleSelect),
                      geo = isolate(input$QueryOptionGeo),
                      category = isolate(input$QueryOptionCategory),   
                      datatype = isolate(input$QueryDataTypeSelect) )       
  }) #------Progress bar end--------
  
  if( is.character(r) ) { # error case
    .GlobalEnv$.ErrorStatus$Query <- TRUE
    callModule( .ErrorMessage, id="QueryErrorMessage", 
                title = "Query:", 
                errormessage = r, 
                idcopy="QueryErrorMessage")
  }
  else { .GlobalEnv$td <- r
    save(td, file=.tdFile )
    .GlobalEnv$.ErrorStatus$Query <- FALSE
  }
}) 

# Help
observeEvent( input$QueryHelpBtn, {
   callModule( .HelpPage,id="QueryHelpPage", 
               helphtmlpage = .HelpHtmlFileName["Query"], 
               helpcss=.HelpCSSFileName, 
               idcopy="QueryHelpPage")
})