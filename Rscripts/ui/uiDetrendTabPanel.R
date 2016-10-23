conditionalPanel( condition ="input.MainTabSet == 'DetrendTabPanel'",
  fluidPage(
     # Error conditinal panel
     .ErrorMessageUI(id="DetrendErrorMessage"),

     # Help conditinal panel 
     .HelpPageUI(id="DetrendHelpPage"), 

     # Detrend Plot
     fluidRow( 
        br(),
        column( width=3,helpText("Detrended plot",style=.SectionsTitle) ),
        column( width=12,plotOutput("DetrendPlot") )
     ),

     # Plot Options
     uiOutput("DetrendPlotOptions")
  )
)