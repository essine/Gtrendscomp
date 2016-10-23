conditionalPanel( condition ="input.MainTabSet == 'DataTabPanel'",

    helpText(strong("Data in table")),
    
    radioButtons( inputId = "DataSelectRBtn", 
                  label = NULL, 
                  choices = c("Original traces"="original"),
                  selected = "original", 
                  inline = FALSE, 
                  width = NULL),
    
    # Apply and Help Buttons
    br(),
    fluidRow(
      column( width=4, 
              downloadButton(outputId="DataDownloadBtn", label="Load", class = "loadbtn"),
              tags$head(tags$style(.DownloadButtonStyle)) ),

      column( width=3, offset=2, actionButton(inputId="DataHelpBtn",label="Help",style = .HelpButtonStyle) )
    ),  
    br(),
    fluidRow(
      column( width=4, offset=0, 
              downloadButton(outputId="DataDownloadAllBtn", label = "Load All", class = "loadbtn"),
              tags$head(tags$style(.DownloadButtonStyle)) )
    )  
)
