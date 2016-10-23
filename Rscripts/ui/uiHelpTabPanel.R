conditionalPanel( condition ="input.MainTabSet == 'HelpTabPanel'",
  includeCSS( .HelpCSSFileName ),
  htmlOutput("HelpPageHtmlOutput")
)
