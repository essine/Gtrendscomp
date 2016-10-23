conditionalPanel( condition ="input.MainTabSet == 'DetrendTabPanel'",
                  
  helpText( strong("Detrend Method") ),
  
  radioButtons( inputId = "DetrendMethodSelectRBtn", 
                  label = NULL, 
                choices = c("Seasonal decomposition"="season","Differencing"="diff","Smoothing"="smooth"), 
               selected = "season", 
                 inline = FALSE, 
                  width = NULL),
  
  # Apply and Help Buttons
  br(),
  fluidRow(
    column( width=4, actionButton(inputId="DetrendApplyBtn",label="Detrend",style = .ApplyButtonStyle) ),
    column( width=3, offset=2, actionButton(inputId="DetrendHelpBtn",label="Help",style = .HelpButtonStyle) )
  )  
)