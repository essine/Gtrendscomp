conditionalPanel( condition ="input.MainTabSet == 'UploadTabPanel'",
                  
  # Upload  csv file
  helpText(strong("CSV file format")),

  # File format
  radioButtons( inputId = "UploadFileFormatSelectRBtn", 
                  label = NULL, 
                choices = c("gtrendscomp CSV" = "gtrendscomp_csv", "custom CSV" = "custom_csv" ), 
               selected = "gtrendscomp_csv", 
                 inline = FALSE, 
                  width = NULL),
  
  # 'Custom csv' conditionalPanel
  conditionalPanel( condition = "input.UploadFileFormatSelectRBtn == 'custom_csv'",
        #helpText(strong("Custom csv")),
        fluidRow(
          column( width=6, helpText("Skip n lines") ),
          column( width=6, numericInput(inputId = "UploadSkipNumeric", label = NULL, value = 1, min = 0, max = 20, width = '100%')  )
        ),
        fluidRow(
          column( width=9, helpText("Set column names") ),
          column( width=1, align="right", checkboxInput("UploadSetNames", label = NULL, value = FALSE ) )
        ),
        conditionalPanel( condition ="input.UploadSetNames == 1",
                          fluidRow(
                            column(width=9,textInput(inputId="UploadSw1",label=NULL,value="Trend1")),
                            column(width=1,checkboxInput("UploadCb1", label = NULL, value=TRUE))
                          ),
                          fluidRow(
                            column(width=9,textInput(inputId="UploadSw2",label=NULL,value="Trend2")),
                            column(width=1,checkboxInput("UploadCb2", label = NULL, value=TRUE))
                          ),
                          fluidRow(
                            column(width=9,textInput(inputId="UploadSw3",label=NULL,value="Trend3")),
                            column(width=1,checkboxInput("UploadCb3", label = NULL, value=FALSE))
                          ),
                          fluidRow(
                            column(width=9,textInput(inputId="UploadSw4",label=NULL,value="Trend4")),
                            column(width=1,checkboxInput("UploadCb4", label = NULL, value=FALSE))
                          ),
                          fluidRow(
                            column(width=9,textInput(inputId="UploadSw5",label=NULL,value="Trend5")),
                            column(width=1,checkboxInput("UploadCb5", label = NULL, value=FALSE))
                          )
        ) # end conditional panel "Set Names"
  ), # end conditional panel "Custom CSV Format"
  
  # Choose file Button
  fileInput( 'UploadFileInput', label = NULL ,
             accept=c('text/csv','text/comma-separated-values,text/plain','.csv'),
             width="100%" ),
    
  # Example checkbox
  fluidRow( 
    column( width=9, helpText(strong("Example CSV")) ),
    column( width=1, checkboxInput(inputId="UploadExample", label = NULL, value = FALSE) )
  ),
  
  # Apply and Help Buttons
  fluidRow( br(),
     column( width=4, actionButton(inputId="UploadApplyBtn",label="Apply",style = .ApplyButtonStyle) ),
     column( width=3, offset=2, actionButton(inputId="UploadHelpBtn",label="Help",style = .HelpButtonStyle) )
  )
)