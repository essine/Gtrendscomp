# Function ".gtmodel_plot" plots model data "tm"
.gtmodel_plot <- function( tm, selectedtraces, selectedcoeff )
{ # "tm" model data to plot, 
  # "selectedtraces" selected traces to plot,
  # "selectedcaeff"  selected trace to plot model coefficients
  
  # check parameters
  if( missing(tm) ) return("no data to plot")
  if( missing(selectedtraces) ) { 
    n <- length(tm$residuals$data) - 1
    selectedtraces <- c( 1:n ) # select all traces
  }
  if( missing(selectedcoeff) ) selectedcoeff <- 1
  
  # save graphical parameters
  oldpar <- par(no.readonly = TRUE)
  
  # Arrange plots
  par(mfrow=c(2,2))

  # Plot selected input data and model residuals
  if( selectedtraces[1] != -1  ){
    .gtquery_plot( tm$residuals, selectedtraces, ylimits=c(2,2,1))
    .gtquery_plot( tm$inputdata, selectedtraces )
  }
  
  # Plot model parameters and LB test data
  .arima_plot ( tm, selectedcoeff )
  .lbtest_plot( tm$lb_test )
  
  # Restore graphical parameters
  par(oldpar) 
  
}

#  source("gtquery_plot.R")
#  load("data/tm.RData")
# .gtmodel_plot(tm)

