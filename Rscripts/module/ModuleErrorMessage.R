#     UI part: .ErrorMessageUI(id="xxx1"), .ErrorMessageUI(id="xxx2")
# Server part: callModule( .ErrorMessage,id="xxx1", title="Error title", errormessage = "This is an error", idcopy="xxx1")
#              callModule( .ErrorMessage,id="xxx2", title="Error title", errormessage = "This is an error", idcopy="xxx2")
#              need a global reactive flag .GlobalEnv$.ErrorStatus[[idcopy]]
#               .GlobalEnv$.ErrorStatus         <- reactiveValues() 
#               .GlobalEnv$.ErrorStatus$xxx1 = 0 ( init value, no error )

#UI module part
.ErrorMessageUI <- function(id) {
   ns <- NS(id)
   uiOutput(ns("ConditinalUIErrorMessageQuery"))
}

# Server module part
.ErrorMessage <- function(input, output,session, title, errormessage, idcopy) 
{ 
  ns <- session$ns

  .ErrorWindowStyle= "
         background-color: rgba(240, 240, 240, 1);
         border-style: solid;
         border-width: 1px; 
         border-color: rgba(200, 0, 0, 0.7); "
  
  .ErrorButtonStyle = "
         color: rgba(200, 0, 0, 0.7);
         border-style: solid;
         border-width: 1px; 
         border-color: rgba(200, 0, 0, 0.7); "
  
  .ErrorMessageStyle = "
         color: rgba(200, 0, 0, 0.7);
         margin-left: 1%"
   
  flag <- reactiveValues( on = TRUE )

  # close error message with "Close Button"
  observeEvent( input$CloseErrorMessageButton, { 
    if( .GlobalEnv$.ErrorStatus[[idcopy]] == 1 ) flag$on <- FALSE 
    else                                         flag$on <- TRUE
  })
  
  output$ConditinalUIErrorMessageQuery<-renderUI ({
    
    if(flag$on  == FALSE)  { 
     .GlobalEnv$.ErrorStatus[[idcopy]]<-FALSE;  return()
    } 
    .GlobalEnv$.ErrorStatus[[idcopy]]<-TRUE
    
    tagList( br(),
       fluidPage (style=.ErrorWindowStyle,
         fluidRow(
             helpText( strong(title), style=.ErrorMessageStyle ),
             p( errormessage, style=.ErrorMessageStyle )
         ),
         fluidRow(
             column( width=12, offset=5, actionButton(inputId=ns("CloseErrorMessageButton"),label="Close please",width="14%",style=.ErrorButtonStyle) )
         )    
       )# End fluidPage
    )# End taglist
  })
} # End server module part