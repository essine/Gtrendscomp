# server.R
shinyServer( function(input, output,session) {

# on a TabPanel selected
observeEvent( input$MainTabSet, {
 if( input$MainTabSet == "DataTabPanel") display_dataframe()    
})

# Query
  source(.ServerFileName["Query"],local=TRUE)$value
  
# Upload
  source(.ServerFileName["Upload"],local=TRUE)$value

# Detrend
  source(.ServerFileName["Detrend"],local=TRUE)$value

# Model
  source(.ServerFileName["Model"],local=TRUE)$value
  
# Compare
  source(.ServerFileName["Compare"],local=TRUE)$value
  
# Data
  source(.ServerFileName["Data"],local=TRUE)$value

# Help
  source(.ServerFileName["Help"],local=TRUE)$value
  
# Exit 
  observeEvent( input$ExitBtn, priority = 1, {
      stopApp()  })  
    
}) # end shinyServer