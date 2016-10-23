# before shiny starts

shinyUI( fluidPage ( style=.ShadowApplication,
 
 tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css"), 
 tags$head(tags$link(rel="shortcut icon", href="www/favicon.png")), # add favicon                  
 
 fluidRow(
   column( width=5, titlePanel(h1("gtrendscomp",style=.ApplicationTitle),windowTitle="gtrendscomp")),
   column( width=2, offset=11, actionButton(inputId="ExitBtn",label="Quite",style = .ExitButtonStyle) )
 ),
  
 sidebarLayout(
  sidebarPanel (width = '2',
      source(.UiSidebarFileName["Query"],   local=TRUE)$value,
      source(.UiSidebarFileName["Upload"],  local=TRUE)$value,
      source(.UiSidebarFileName["Detrend"], local=TRUE)$value,
      source(.UiSidebarFileName["Model"],   local=TRUE)$value,
      source(.UiSidebarFileName["Compare"], local=TRUE)$value,
      source(.UiSidebarFileName["Data"],    local=TRUE)$value,
      source(.UiSidebarFileName["Help"],    local=TRUE)$value
  ), # end sidebarPanel
  mainPanel (width=10,
   tabsetPanel( id = "MainTabSet",
                
    tabPanel( title="Query", value = "QueryTabPanel",
      source(.UiTabPanelFileName["Query"], local=TRUE)$value        
    ), # end tabPanel Query
    
    tabPanel( title="Upload", value = "UploadTabPanel",
      source(.UiTabPanelFileName["Upload"], local=TRUE)$value        
    ), # end tabPanel Upload

    tabPanel( title="Detrend", value = "DetrendTabPanel",
      source(.UiTabPanelFileName["Detrend"], local=TRUE)$value        
    ), # end tabPanel Detrend
    
    tabPanel( title="Model", value = "ModelTabPanel",
      source(.UiTabPanelFileName["Model"], local=TRUE)$value        
    ), # end tabPanel Model

    tabPanel( title="Compare", value = "CompareTabPanel",
      source(.UiTabPanelFileName["Compare"], local=TRUE)$value        
    ), # end tabPanel Compare

    tabPanel( title="Data", value = "DataTabPanel",
      source(.UiTabPanelFileName["Data"], local=TRUE)$value        
    ), # end tabPanel Data

    tabPanel( title="Help", value = "HelpTabPanel",
      source(.UiTabPanelFileName["Help"], local=TRUE)$value        
    ) # end tabPanel Help

   ) # end tabsetPanel
  )# end mainPanel
 ) # end sidebarlayout
) # end fluidPage
) # end shinyUI



