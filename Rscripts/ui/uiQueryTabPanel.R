conditionalPanel( condition ="input.MainTabSet == 'QueryTabPanel'",
  fluidPage(
     # Error conditinal panel
     .ErrorMessageUI(id="QueryErrorMessage"),

     # Help conditinal panel 
     .HelpPageUI(id="QueryHelpPage"), 

     # Query Plot
     fluidRow( 
        br(),
        column( width=3,helpText("Google trends plot",style=.SectionsTitle) ),
        column( width=12,plotOutput("QueryPlot") )
     ),

     # Plot Options
     uiOutput("QueryPlotOptions")
 )
)