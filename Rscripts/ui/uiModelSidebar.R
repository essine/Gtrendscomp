conditionalPanel( condition ="input.MainTabSet == 'ModelTabPanel'",
                  
  helpText( strong("Data to model") ),
  
  radioButtons( inputId = "ModelDataSelectRBtn", 
                  label = NULL, 
                choices = c("Original traces"="original"),
               selected = "original", 
                 inline = FALSE, 
                  width = NULL),
  
  # Apply and Help Buttons
  br(),
  fluidRow(
    column( width=4, actionButton(inputId="ModelApplyBtn",label="Model",style = .ApplyButtonStyle) ),
    column( width=3, offset=2, actionButton(inputId="ModelHelpBtn",label="Help",style = .HelpButtonStyle) )
  )  
)