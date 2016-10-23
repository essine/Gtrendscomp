#     UI part:     .HelpPageUI(id="xxx1"), .HelpPageUI(id="xxx2")
#     Server part:  callModule( .HelpPage,id="xxx1", helphtmlpage = "www/helppage1.html", helpcss="www/help.css", idcopy="xxx1")
#                   callModule( .HelpPage,id="xxx2", helphtmlpage = "www/helppage2.html", helpcss="www/help.css", idcopy="xxx1")
#              need a global reactive flag .GlobalEnv$.HelpStatus[[idcopy]]
#               .GlobalEnv$.HelpStatus         <- reactiveValues() 
#               .GlobalEnv$.HelpStatus$xxx1 = 0 ( init value, help window off )


#UI module part
.HelpPageUI <- function(id) {
   ns <- NS(id)
   uiOutput(ns("ConditinalUIHelpPage"))
}

# Server module part
.HelpPage <- function(input, output,session, helphtmlpage, helpcss, idcopy) 
{ 
  ns <- session$ns

  .HelpWindowStyle= "
         background-color: rgba(240,240,240,1);
         border-style: solid;
         border-width: 1px; 
         border-color: rgba(0, 0, 255, 1); "
  
  .HelpButtonStyle = "
         color: rgba(0, 0, 255, 1);
         border-style: solid;
         border-width: 1px; 
         border-color: rgba(0, 0, 255, 1); "

  .HelpTextStyle = "
         color: rgba(0, 0, 255, 1);
         margin-left: 1%"

  flag <- reactiveValues( on = TRUE )

  # close help window with "Close Button"
  observeEvent( input$CloseHelpPageButton, { 
    if( .GlobalEnv$.HelpStatus[[idcopy]] == 1 ) flag$on <- FALSE 
    else                                        flag$on <- TRUE
  })

  output$ConditinalUIHelpPage <- renderUI ({
    
  if(flag$on  == FALSE)  { 
    .GlobalEnv$.HelpStatus[[idcopy]]<-FALSE;  return()
  } 
  .GlobalEnv$.HelpStatus[[idcopy]]<-TRUE

  tagList( br(),  
    fluidPage (style=.HelpWindowStyle,
      fluidRow(
          includeCSS ( helpcss ),
          includeHTML( helphtmlpage )
      ),
      fluidRow(
          column( width=12, offset=5, actionButton(inputId=ns("CloseHelpPageButton"),label="Close",width="14%",style=.HelpButtonStyle) )
      ) 
    )# End fluidPage
  )# End tagList
  })
} # End server module part