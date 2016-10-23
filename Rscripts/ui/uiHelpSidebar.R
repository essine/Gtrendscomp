conditionalPanel( condition ="input.MainTabSet == 'HelpTabPanel'",

    helpText( strong("Help Section") ),
              
    radioButtons( inputId = "HelpPageSelectRBtn", 
                    label = NULL, 
                  choices = .HelpPageChoices, 
                 selected = .HelpPageSelected, 
                   inline = FALSE, 
                    width = NULL)
 )
