conditionalPanel( condition ="input.MainTabSet == 'DataTabPanel'",

    fluidPage(
        # Error conditinal panel
        .ErrorMessageUI(id="DataErrorMessage"),
                    
        # Help conditinal panel 
        .HelpPageUI(id="DataHelpPage"), 
                    
        # Data Plot
        fluidRow( 
          br(),
          column( width=8,textOutput("DataTitle"),style=.SectionsTitle),
          column( width=12,dataTableOutput("DataAsTable") )
        )
    )              
)
