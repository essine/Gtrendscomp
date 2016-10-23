# Plot
.GenerateUploadPlot <- function() {
    r = .gtquery_plot(.GlobalEnv$td )
    if(is.character(r)) { # error case
      callModule( .ErrorMessage, id="UploadErrorMessage", 
                  title = "Upload plot:", 
                  errormessage = r, 
                  idcopy="UploadErrorMessage")
   }
}

# Upload Plot
output$UploadPlot <- renderPlot({
  if(input$UploadApplyBtn == 0)  plot.new()
  else  { 
    if(.GlobalEnv$.ErrorStatus$Upload == FALSE) .GenerateUploadPlot()
    else plot.new()
  }
})

# Upload csv file data
.upload_csv <- function() {
      inFile <- input$UploadFileInput
      if (is.null(inFile)) return(NULL)

      .GlobalEnv$.csvdata <- data.frame()
      csvformat = isolate(input$UploadFileFormatSelectRBtn) 
      output$UploadDataTitle <- renderText ({ paste("CSV data: ",inFile$name) })
      switch(csvformat,
         "gtrendscomp_csv" = { 
             .GlobalEnv$.csvdata <- read.csv(inFile$datapath)     
         },
         "custom_csv"      = 
         {   
             .GlobalEnv$.csvdata <- read.csv(inFile$datapath, skip = input$UploadSkipNumeric) 
             if(isolate(input$UploadSetNames)) set_upload_colnames()#set name is ON 
         }
      )
      return (.GlobalEnv$.csvdata )
}

# set uploaded data column names
set_upload_colnames <- function()
{
  names(.GlobalEnv$.csvdata)[1] <- "Date"
  cbx_names <- c(isolate(input$UploadCb1), isolate(input$UploadCb2), isolate(input$UploadCb3), isolate(input$UploadCb4), isolate(input$UploadCb5))   
  sw_names <- c(isolate(input$UploadSw1), isolate(input$UploadSw2), isolate(input$UploadSw3), isolate(input$UploadSw4), isolate(input$UploadSw5))   
  for( i in 1:5 ) if(cbx_names[i]) #replace name[i] if cbx is TRUE
    if(i < length(.GlobalEnv$.csvdata)) names(.GlobalEnv$.csvdata)[i+1] <- sw_names[i]
}  

# Observe csv file upload
observeEvent ( input$UploadFileInput, {
  output$UploadAsTable <- renderDataTable ({   .upload_csv() }, options = list( pageLength = 5 ))
})

# "Example csv" checkbox
observeEvent ( input$UploadExample, {
  if(input$UploadExample) { # checkbox selected
  .GlobalEnv$.csvdata <- data.frame()
  .GlobalEnv$.csvdata <- read.csv("data/csv/gtrendscomp.csv")  
   output$UploadDataTitle <- renderText ({ paste("CSV file example ('gtrendscomp csv' file format): gtrendscomp.csv") })
   output$UploadAsTable <- renderDataTable ({   .GlobalEnv$.csvdata }, options = list( pageLength = 5 ) )
         
   output$UploadPlot <- renderPlot({
      .convertcsv()
      if(.GlobalEnv$.ErrorStatus$Upload == FALSE) .GenerateUploadPlot()
   })
  } 
})

# Convert 'csv' data to 'td' data
.convertcsv <- function() {
  
  .GlobalEnv$td$title      <- "Interest over time"
  .GlobalEnv$td$subtitle   <- ""
  .GlobalEnv$td$query      <- names( .GlobalEnv$.csvdata[-1] )  
  x <- as.Date(as.matrix(.GlobalEnv$.csvdata[1])) 
  .GlobalEnv$td$data       <- data.frame(Date=x,.GlobalEnv$.csvdata[-1])
  
}

# Apply
observeEvent( input$UploadApplyBtn, priority = 1, {
  if( is.null(.GlobalEnv$.csvdata ) )
  {
    .GlobalEnv$.ErrorStatus$Upload = TRUE
    callModule( .ErrorMessage, id="UploadErrorMessage", 
                title = "Upload:", 
                errormessage = "data are not uploaded", 
                idcopy="UploadErrorMessage")
  }
  else { 
    .convertcsv()
    .GlobalEnv$.ErrorStatus$Upload = FALSE 
  }
})

# Help
observeEvent( input$UploadHelpBtn, {
   callModule( .HelpPage,id="UploadHelpPage", 
               helphtmlpage = .HelpHtmlFileName["Upload"], 
               helpcss=.HelpCSSFileName, 
               idcopy="UploadHelpPage")
})
