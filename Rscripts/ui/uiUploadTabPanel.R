conditionalPanel( condition ="input.MainTabSet == 'UploadTabPanel'",
  fluidPage(
     # Error conditinal panel
     .ErrorMessageUI(id="UploadErrorMessage"),

     # Help conditinal panel 
     .HelpPageUI(id="UploadHelpPage"), 

     # Upload Table
     fluidRow( 
       column( width=8,textOutput("UploadDataTitle"),style=.SectionsTitle)
     ),
     dataTableOutput("UploadAsTable"),
     
     # Upload Plot
     fluidRow( 
       column( width=3,helpText("CSV plot",style=.SectionsTitle) ),
       column( width=12,plotOutput("UploadPlot") )
     )
 )
)