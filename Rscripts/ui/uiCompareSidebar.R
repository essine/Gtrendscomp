conditionalPanel( condition = "input.MainTabSet == 'CompareTabPanel'",

    helpText( strong("Data to compare") ),

    radioButtons( inputId = "CompareDataSelectRBtn", 
                  label = NULL, 
                choices = c("Original traces"="original"),
               selected = "original", 
                 inline = FALSE, 
                  width = NULL),

    helpText( strong("Comparison method") ),

    radioButtons( inputId = "CompareMethodSelectRBtn", 
                  label = NULL, 
                choices = c("Autocorrelation"="autocorr", "Crosscorrelation"="crosscorr", "Synchrony"="synchrony"),
               selected = "autocorr", 
                 inline = FALSE, 
                  width = NULL),

    conditionalPanel( condition = "input.CompareMethodSelectRBtn == 'autocorr'",
        helpText("Confidence bands"),
        selectInput(width="100%",inputId="CompareCBandsSelect",label=NULL,
                    choices = list("Standard formula" = "Standard formula", "Bartlett's formula" = "Bartlett's formula"),
                    selected = "Standard formula")
    ),
	
    # Apply and Help Buttons
    br(),
    fluidRow(
      column( width=4, actionButton(inputId="CompareApplyBtn",label="Compare",style = .ApplyButtonStyle) ),
      column( width=3, offset=2, actionButton(inputId="CompareHelpBtn",label="Help",style = .HelpButtonStyle) )
    )
)
