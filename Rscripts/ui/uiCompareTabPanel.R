conditionalPanel( condition ="input.MainTabSet == 'CompareTabPanel'",
  fluidPage(
     # Error conditinal panel
     .ErrorMessageUI(id="CompareErrorMessage"),

     # Help conditinal panel 
     .HelpPageUI(id="CompareHelpPage"), 

     # Compare Plot
     fluidRow( 
        br(),
        column( width=3, helpText("Comparison plot",style=.SectionsTitle) ),
        column( width=12, plotOutput("ComparePlot") )
     ),

     # Plot Options
     uiOutput("ComparePlotOptions")
  )              
)
