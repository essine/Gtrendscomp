output$HelpPageHtmlOutput <- renderUI({ # htmlOutput, tabPanel "Help"
 includeHTML( 
  .HelpHtmlFileName[as.character( input$HelpPageSelectRBtn )]
 )
})