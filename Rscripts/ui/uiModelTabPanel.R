conditionalPanel( condition ="input.MainTabSet == 'ModelTabPanel'",
  fluidPage(
     # Error conditinal panel
     .ErrorMessageUI(id="ModelErrorMessage"),

     # Help conditinal panel 
     .HelpPageUI(id="ModelHelpPage"), 

     # Model Plot
     fluidRow( 
        br(),
        column( width=3,helpText("Model plot",style=.SectionsTitle) ),
        column( width=12,plotOutput("ModelPlot") )
     ),

     # Plot Options
     uiOutput("ModelPlotOptions")
  )
)