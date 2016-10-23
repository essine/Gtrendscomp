conditionalPanel( condition ="input.MainTabSet == 'QueryTabPanel'",
                  
 helpText( strong("Google Account") ),
 
 fluidRow( # email, EML
    column(width=3, helpText("EML: ")),
    column(width=9, textInput( inputId="QueryUser",label=NULL,value=.usr_value ))
 ),
 fluidRow( # password, PWD
    column(width=3, helpText("PWD: ")),
    column(width=9, passwordInput( inputId="QueryPassword", label=NULL,value=.pwd_value))
 ),

 # Search terms group
 fluidRow(
    column( width=9,helpText( strong("Search terms") ) ),
    column( width=3,checkboxInput("QueryMoreTrends", label = NULL, value = .moreTrends ) )
 ),

 fluidRow(
    column(width=9,textInput(inputId="QuerySw1",label=NULL,value=.lsr$value[1])),
    column(width=1,checkboxInput("QueryCb1", label = NULL, value=.lsr$boxvalue[1]))
 ),

 fluidRow(
    column(width=9,textInput(inputId="QuerySw2",label=NULL,value=.lsr$value[2])),
    column(width=1,checkboxInput("QueryCb2", label = NULL, value=.lsr$boxvalue[2]))
 ),

 conditionalPanel ( condition="input.QueryMoreTrends",
    fluidRow(
       column(width=9,textInput(inputId="QuerySw3",label=NULL,value=.lsr$value[3])),
       column(width=1,checkboxInput("QueryCb3", label = NULL, value=.lsr$boxvalue[3]))
    ),
    fluidRow(
       column(width=9,textInput(inputId="QuerySw4",label=NULL,value=.lsr$value[4])),
       column(width=1,checkboxInput("QueryCb4", label = NULL, value=.lsr$boxvalue[4]))
    ),
    fluidRow(
       column(width=9,textInput(inputId="QuerySw5",label=NULL,value=.lsr$value[5])),
       column(width=1,checkboxInput("QueryCb5", label = NULL, value=.lsr$boxvalue[5]))
    )
 ),# end conditionalPanel "MoreTrends"

 # Date range
 fluidRow(
    column( width=9, helpText(strong("Date range")) ),
    column( width=1,checkboxInput("QueryDateRange", label = NULL, value = FALSE ) )
 ),
 conditionalPanel ( condition="input.QueryDateRange",
   fluidRow( 
      column(width=3, helpText("From:")),
      column(width=9, dateInput(width="100%",inputId="QueryStartDate",label=NULL,value=.sdate))
   ),
   fluidRow(
      column( width=3, helpText("To:")),
      column( width=9, dateInput(width="100%",inputId="QueryEndDate",label=NULL,value=.edate) )
   )
 ),# end conditionalPanel "QueryDateRange"

 # Query options
 fluidRow(
    column( width=9, helpText(strong("Options")) ),
    column( width=1,checkboxInput("QueryOptions", label = NULL, value = FALSE ) )
 ),
 conditionalPanel ( condition="input.QueryOptions",
   fluidRow(
      column( width=5, helpText("Scale:")),
      column( width=7, selectInput(width="100%",inputId="QueryOptionScaleSelect",label=NULL,
                                   choices = list("NA" = "NA", "1h" = "1h", "4h" = "4h", "1d" = "1d", "7d" = "7d"),
                                   selected = "NA") )
   ),
   fluidRow(
      column( width=3, helpText("Geo:")),
      column( width=9, textInput(width="100%",inputId="QueryOptionGeo",label=NULL,value="all") )
   ),
   fluidRow(
      column( width=5, helpText("Category:")),
      column( width=7, textInput(width="100%",inputId="QueryOptionCategory",label=NULL,value="0") )
   )
 ), # end conditionalPanel "Options"

 # Data type selection
  helpText(strong("Data type")),
  selectInput(inputId = "QueryDataTypeSelect", label = NULL, 
              choices = list("Google original" = "google_original", 
                             "Equally scaled"  = "equally_scaled", 
                             "Improved data"   = "improved_precision"),
              selected = "google_original"),

 # Offline Example
 fluidRow( 
    column( width=9, helpText(strong("Example data")) ),
    column( width=1, checkboxInput(inputId="QueryExample", label = NULL, value = .example) )
 ),

 # Apply and Help Buttons
 fluidRow(
    column( width=4, actionButton(inputId="QueryApplyBtn",label="Query",style = .ApplyButtonStyle) ),
    column( width=3, offset=2, actionButton(inputId="QueryHelpBtn",label="Help",style = .HelpButtonStyle) )
 )
)