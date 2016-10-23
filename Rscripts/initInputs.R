# *********** P A G E  N A M E ****
.ApplicationPageName = c( "Query","Upload","Detrend","Model","Compare","Data","Help" )

# *********** I D  ****************
# tabsetPanel        "MainTabSet"                     in ui.R
# tabPanel           "QueryTabPanel"                  in ui.R
# tabPanel           "DetrendTabPanel"                in ui.R
# tabPanel           "ModelTabPanel"                  in ui.R
# tabPanel           "CompareTabPanel"                in ui.R
# tabPanel           "DataTabPanel"                   in ui.R
# tabPanel           "HelpTabPanel"                   in ui.R

# plotOutput         "QueryPlot"                      in uiQueryTabPanel.R
# uiOutput           "QueryPlotOptions"               in uiQueryTabPanel.R
# .HelpPageUI        "QueryHelpPage"                  in uiQueryTabPanel.R
# .ErrorMessageUI    "QueryErrorMessage",             in uiQueryTabPanel.R
# checkboxGroupInput "QueryPlotSelectTraces"          in ServerQuery.R
# textinput          "QueryUser"                      in uiQuerySiderbar.R
# passwordInput      "QueryPassword"                  in uiQuerySiderbar.R
# checkboxInput      "QueryMoreTrends"                in uiQuerySiderbar.R
# textInput          "QuerySw1"                       in uiQuerySiderbar.R
# textInput          "QuerySw2"                       in uiQuerySiderbar.R
# textInput          "QuerySw3"                       in uiQuerySiderbar.R
# textInput          "QuerySw4"                       in uiQuerySiderbar.R
# textInput          "QuerySw5"                       in uiQuerySiderbar.R
# checkboxInput      "QueryCb1"                       in uiQuerySiderbar.R
# checkboxInput      "QueryCb2"                       in uiQuerySiderbar.R
# checkboxInput      "QueryCb3"                       in uiQuerySiderbar.R
# checkboxInput      "QueryCb4"                       in uiQuerySiderbar.R
# checkboxInput      "QueryCb5"                       in uiQuerySiderbar.R
# checkboxInput      "QueryDateRange"                 in uiQuerySiderbar.R
# dateInput          "QueryStartDate"                 in uiQuerySiderbar.R
# dateInput          "QueryEndDate"                   in uiQuerySiderbar.R
# checkboxInput      "QueryOptions"                   in uiQuerySiderbar.R
# selectInput        "QueryOptionScaleSelect"         in uiQuerySiderbar.R
# textInput          "QueryOptionGeo"                 in uiQuerySiderbar.R
# textInput          "QueryOptionCategory"            in uiQuerySiderbar.R
# checkboxInput      "QueryExample"                   in uiQuerySiderbar.R
# selectInput        "QueryDataTypeSelect"            in uiQuerySiderbar.R
# actionButton       "QueryApplyBtn"                  in uiQuerySiderbar.R
# actionButton       "QueryHelpBtn"                   in uiQuerySiderbar.R

# radioButtons       "UploadFileFormatSelectRBtn"     in uiUploadSiderbar.R
# fileInput          "UploadFileInput"                in uiUploadSiderbar.R
# numericInput       "UploadSkipNumeric"              in uiUploadSiderbar.R
# checkboxInput      "UploadSetNames"                 in uiUploadSiderbar.R
# textInput          "UploadSw1"                      in uiUploadSiderbar.R
# textInput          "UploadSw2"                      in uiUploadSiderbar.R
# textInput          "UploadSw3"                      in uiUploadSiderbar.R
# textInput          "UploadSw4"                      in uiUploadSiderbar.R
# textInput          "UploadSw5"                      in uiUploadSiderbar.R
# checkboxInput      "UploadCb1"                      in uiUploadSiderbar.R
# checkboxInput      "UploadCb2"                      in uiUploadSiderbar.R
# checkboxInput      "UploadCb3"                      in uiUploadSiderbar.R
# checkboxInput      "UploadCb4"                      in uiUploadSiderbar.R
# checkboxInput      "UploadCb5"                      in uiUploadSiderbar.R
# checkboxInput      "UploadExample"                  in uiUploadSiderbar.R 
# actionButton       "UploadApplyBtn"                 in uiUploadSiderbar.R
# actionButton       "UploadHelpBtn"                  in uiUploadSiderbar.R
# .HelpPageUI        "UploadHelpPage"                 in uiUploadTabPanel.R
# .ErrorMessageUI    "UploadErrorMessage"             in uiUploadTabPanel.R
# plotOutput         "UploadPlot"                     in uiUploadTabPanel.R
# dataTableOutput    "UploadAsTable"                  in uiUploadTabPanel.R
# textOutput         "UploadDataTitle"                in uiUploadTabPanel.R

# radioButtons       "DetrendMethodSelectRBtn"        in uiDetrendSiderbar.R
# actionButton       "DetrendApplyBtn"                in uiDetrendSiderbar.R
# actionButton       "DetrendHelpBtn"                 in uiDetrendSiderbar.R
# .HelpPageUI        "DetrendHelpPage"                in uiDetrendTabPanel.R
# .ErrorMessageUI    "DetrendErrorMessage"            in uiDetrendTabPanel.R
# plotOutput         "DetrendPlot"                    in uiDetrendTabPanel.R
# uiOutput           "DetrendPlotOptions"             in uiDetrendTabPanel.R
# checkboxGroupInput "DetrendPlotSelectTraces"        in uiDetrendTabPanel.R, updated in ServerDetrend.R

# radioButtons       "ModelDataSelectRBtn"            in uiModelSiderbar.R, updated in ServerDetrend.R
# actionButton       "ModelApplyBtn"                  in uiModelSiderbar.R
# actionButton       "ModelHelpBtn"                   in uiModelSiderbar.R
# .HelpPageUI        "ModelHelpPage"                  in uiModelTabPanel.R
# .ErrorMessageUI    "ModelErrorMessage"              in uiModelTabPanel.R
# plotOutput         "ModelPlot"                      in uiModelTabPanel.R
# uiOutput           "ModelPlotOptions"               in uiModelTabPanel.R, updated in ServerModel.R 
# checkboxGroupInput "ModelPlotSelectTraces"          in uiModelTabPanel.R, updated in ServerModel.R 
# radioButtons       "ModelPlotSelectCoeff"           in uiModelTabPanel.R, updated in ServerModel.R 

# radioButtons       "CompareDataSelectRBtn"          in uiCompareSiderbar.R, updated in ServerDetrend.R, ServerModel.R
# radioButtons       "CompareMethodSelectRBtn"        in uiCompareSiderbar.R
# selectInput        "CompareCBandsSelect"            in uiCompareSiderbar.R
# actionButton       "CompareApplyBtn"                in uiCompareSiderbar.R
# actionButton       "CompareHelpBtn"                 in uiCompareSiderbar.R
# plotOutput         "ComparePlot"                    in uiCompareTabPanel.R
# uiOutput           "ComparePlotOptions"             in uiCompareTabPanel.R, updated in ServerCompare.R
# checkboxGroupInput "ComparePlotSelectTraces"        in uiCompareTabPanel.R, updated in ServerCompare.R 
# radioButtons       "ComparePlotSelectACFRBtn"       in uiCompareTabPanel.R, updated in ServerCompare.R 
# checkboxGroupInput "ComparePlotSelectCCF"           in uiCompareTabPanel.R, updated in ServerCompare.R 
# radioButtons       "ComparePlotSelectCmRBtn"        in uiCompareTabPanel.R, updated in ServerCompare.R 

# radioButtons       "DataSelectRBtn"                 in uiDataSiderbar.R,  updated in ServerDetrend.R, ServerModel.R, ServerCompare.R
# actionButton       "DataHelpBtn"                    in uiDataSiderbar.R
# downloadButton     "DataDownloadBtn"                in uiDataSiderbar.R
# downloadButton     "DataDownloadAllBtn"             in uiDataSiderbar.R
# .HelpPageUI        "DataHelpPage"                   in uiDataTabPanel.R
# .ErrorMessageUI    "DataErrorMessage"               in uiDataTabPanel.R
# dataTableOutput    "DataAsTable"                    in uiDataTabPanel.R
# textOutput         "TableTitle"                     in uiDataTabPanel.R

# radioButtons       "HelpPageSelectRBtn"             in uiHelpSidebar.R
# htmlOutput         "HelpPageHtmlOutput"             in uiHelpTabPanel.R

# ***********P A T H S**************
.DataDir         = "data/"
.DataExampleDir  = "data/example/"
.HtmlFilesDir    = "www/"

.RFilesDir = "Rscripts/"
.RFilesModuleDir = paste(.RFilesDir,"module/",      sep="")
.RFilesServerDir = paste(.RFilesDir,"server/",      sep="")
.RFilesUIDir     = paste(.RFilesDir,"ui/",          sep="")
.RFilesAPDir     = paste(.RFilesDir,"application/", sep="") 

# *********** F I L E  N A M E ****
.tdFile = paste(.DataDir,"td.RData",sep="")               # td trace data            
.tdExampleFile = paste(.DataExampleDir,"td.RData",sep="") # td example data
.tddFile = paste(.DataDir,"tdd.RData",sep="")             # tdd detrended data
.tmFile = paste(.DataDir,"tm.RData",sep="")               # tm model data
.tcFile = paste(.DataDir,"tc.RData",sep="")               # tc comparison data
.tableFile = paste(.DataDir,"table.RData",sep="")         # table data

.HelpCSSFileName = paste(.HtmlFilesDir,"help.css",sep="")

.HelpPageChoices  = c("About","Query","Upload","Detrend","Model","Compare","Data")
.HelpHtmlFileName = paste(.HtmlFilesDir,.HelpPageChoices,".html",sep="")
names(.HelpHtmlFileName) <- .HelpPageChoices

.ServerFileName = paste(.RFilesServerDir,"Server",.ApplicationPageName,".R",sep="")
names(.ServerFileName) <- .ApplicationPageName

.UiSidebarFileName = paste(.RFilesUIDir,"ui",.ApplicationPageName,"Sidebar.R",sep="")
names(.UiSidebarFileName) <- .ApplicationPageName

.UiTabPanelFileName = paste(.RFilesUIDir,"ui",.ApplicationPageName,"TabPanel.R",sep="")
names(.UiTabPanelFileName) <- .ApplicationPageName

# ***********D A T A  F I L E S*****
td  <- NULL  # trace data
tdd <- NULL  # detrended data
tm  <- NULL  # model data
tc  <- NULL  # comparison data

# ***********Q U E R Y**************
if(exists("GOOGLE_USER")) .usr_value = GOOGLE_USER else .usr_value="user@gmail.com"   #inputId="QueryUsr"
if(exists("GOOGLE_PASSWORD")) .pwd_value = GOOGLE_PASSWORD else .pwd_value="password" #inputId="QueryPwd"

.lsr = list(  # lsr search words and checkbox
  id=c("QuerySw1","QuerySw2","QuerySw3","QuerySw4","QuerySw5"),
  value=c("Search1","Search2","Search3","Search4","Search5"),
  boxid=c("QueryCb1","QueryCb2","QueryCb3","QueryCb4","QueryCb5"),
  boxvalue=c(TRUE,TRUE,FALSE,FALSE,FALSE)
)
.moreTrends    = FALSE   #checkboxInput "QueryMoreTrends"

# inputId="QueryStartDate", inputId="QueryEndDate"
# as.Date("2004-01-01")      as.Date( Sys.time() )
.sdate = as.Date("2004-01-01") # must be greater than 2004-01-01, see help for gtrends()
.edate   = as.Date(Sys.time())

.example  = FALSE   # checkboxInput "QueryExample" -> load example data
.cutquerysize = 7   # create a label from the fist 7th symbols of a query
.shinyappsio = FALSE # 'FALSE' if application is running locally and 'TRUE' if remotely, at shinyapps.io 

# ***********U P L O A D**********
.cvsdata <- NULL # uploaded cvs file data

# ***********M O D E L************
.modeldatachoices <- list( 
  possible = list(
    value= c("Original traces" = "original",                             
             "Remainder components (seasonal decomposition)" = "seasonal_remainder",
             "Differenced data" = "differenced",
             "Remainder components (smoothing)" = "smoothed_remainder"),

    flag = c("Original traces" = 1,                             
             "Remainder components (seasonal decomposition)" = 0,
             "Differenced data" = 0,
             "Remainder components (smoothing)" = 0 )
  ),
  actual = c("Original traces"="original") # init choices
)

# ***********C O M P A R E********
.comparedatachoices <- list( 
  possible = list(
    value= c("Original traces" = "original",                             
             "Seasonal components (seasonal decomposition)" = "seasonal",
             "Trend components (seasonal decomposition)" = "seasonal_trend",
             "Remainder components (seasonal decomposition)" = "seasonal_remainder",
             "Differenced data" = "differenced",
             "Smoothed data" = "smoothed",
             "Remainder components (smoothing)" = "smoothed_remainder",
             "Model Residuals"="model_residuals" ),

    flag = c("Original traces" = 1,                             
             "Seasonal components (seasonal decomposition)" = 0,
             "Trend components (seasonal decomposition)" = 0,
             "Remainder components (seasonal decomposition)" = 0,
             "Differenced data" = 0,
             "Smoothed data" = 0,
             "Remainder components (smoothing)" = 0,
             "Model Residuals"= 0 )
  ),
  actual = c("Original traces"="original") # init choices
)
# ***********D A T A**************
.viewdatachoices <- list( 
  possible = list(
    value= c("Original traces" = "original",                             
             "Seasonal components (seasonal decomposition)" = "seasonal",
             "Trend components (seasonal decomposition)" = "seasonal_trend",
             "Remainder components (seasonal decomposition)" = "seasonal_remainder",
             "Differenced data" = "differenced",
             "Smoothed data" = "smoothed",
             "Remainder components (smoothing)" = "smoothed_remainder",
             "Model Residuals"="model_residuals",
             "Model Parameters"="model_parameters",
             "Model LB Test" = "model_lbtest", 
             "Autocorr. sgn." = "acorr_sgn",
             "Crosscorr. lag0" = "ccorr_lag0",
             "Crosscorr. max." = "ccorr_max",
             "Crosscorr. max. lag" = "ccorr_max_lag",
             "Synchrony" = "synchrony"
            ),

    flag = c("Original traces" = 1,                             
             "Seasonal components (seasonal decomposition)" = 0,
             "Trend components (seasonal decomposition)" = 0,
             "Remainder components (seasonal decomposition)" = 0,
             "Differenced data" = 0,
             "Smoothed data" = 0,
             "Remainder components (smoothing)" = 0,
             "Model Residuals"= 0,
             "Model Parameters" = 0,
             "Model LB Test" = 0,
             "Autocorr. sgn." =  0,
             "Crosscorr. lag0" = 0,
             "Crosscorr. max." = 0,
             "Crosscorr. max. lag" = 0,
             "Synchrony" = 0
            )
  ),
  actual = c("Original traces"="original") # init choices
)

# ***********H E L P**************
#radioButtons "HelpPageSelectRBtn"
  #.HelpPageChoices  = c("About","Query","Upload","Detrend","Model","Compare","Data")
  .HelpPageSelected = "About"  # first page

   # Flags reserved by .HelpPage module
  .GlobalEnv$.HelpStatus         <- reactiveValues() 
  .GlobalEnv$.HelpStatus$QueryHelpPage   <- 0 # Flag that helps to close help window
  .GlobalEnv$.HelpStatus$UploadHelpPage  <- 0 # Flag that helps to close help window
  .GlobalEnv$.HelpStatus$DetrendHelpPage <- 0 # Flag that helps to close help window
  .GlobalEnv$.HelpStatus$ModelHelpPage   <- 0 # Flag that helps to close help window
  .GlobalEnv$.HelpStatus$CompareHelpPage <- 0 # Flag that helps to close help window
  .GlobalEnv$.HelpStatus$DataHelpPage    <- 0 # Flag that helps to close help window


# ***********E R R O R S**************
  .GlobalEnv$.ErrorStatus         <- reactiveValues() 
  # Flags reserved by .ErrorMessage module
  .GlobalEnv$.ErrorStatus$QueryErrorMessage   <- 0 # Flag that helps to close error window
  .GlobalEnv$.ErrorStatus$UploadErrorMessage  <- 0 # Flag that helps to close error window
  .GlobalEnv$.ErrorStatus$DetrendErrorMessage <- 0 # Flag that helps to close error window
  .GlobalEnv$.ErrorStatus$ModelErrorMessage   <- 0 # Flag that helps to close error window
  .GlobalEnv$.ErrorStatus$CompareErrorMessage <- 0 # Flag that helps to close error window
  .GlobalEnv$.ErrorStatus$DataErrorMessage    <- 0 # Flag that helps to close error window

  .GlobalEnv$.ErrorStatus$Query   <- 0
  .GlobalEnv$.ErrorStatus$Upload  <- 0 # Flag that indicates error status
  .GlobalEnv$.ErrorStatus$Detrend <- 0 # Flag that indicates error status
  .GlobalEnv$.ErrorStatus$Model   <- 0 # Flag that indicates error status
  .GlobalEnv$.ErrorStatus$Compare <- 0 # Flag that indicates error status
  .GlobalEnv$.ErrorStatus$Data    <- 0 # Flag that indicates error status

# ***********U T I L S****************
.load_rscripts <- function(dir_name) { # dir_name should ends with '/'
   lf = list.files(dir_name)
   for(i in 1:length(lf)) source(paste(dir_name,lf[i],sep=""))
}
