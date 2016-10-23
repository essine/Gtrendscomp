# Function .gtdetrend_plot plots selected detrended traces

.gtdetrend_plot <- function(tdd, selected)
{ # return "error message" or nothing 
  # "tdd" detrended data to plot
  # "selected" traces to plot c(1,3), if missed select all

  if( missing(tdd) ) return("no data to plot")
  if( missing(selected) ) { n <- length(tdd$original$data) - 1
    selected <- c( 1:n ) # select all traces
  }  
  
  
  # save graphical parameters
  oldpar <- par(no.readonly = TRUE)
  
  # Arrange plots
  par(mfrow=c(2,2))
  
  # Plot selected
  switch( tdd$title,
    "Seasonal decomposition" = {
      .gtquery_plot( tdd$seasonal, selected)
      .gtquery_plot( tdd$trend,    selected)
      .gtquery_plot( tdd$remainder,selected,ylimits=c(2,2,1))
      .gtquery_plot( tdd$original, selected)
    },
    "Smoothing" = {
      .gtquery_plot( tdd$smoothed, selected)
      .gtquery_plot( tdd$remainder, selected,ylimits=c(2,2,1))
      .gtquery_plot( tdd$original, selected)
    },
    "Differencing" = {
      .gtquery_plot( tdd$diff, selected,ylimits=c(2,2,1))
      .gtquery_plot( tdd$original, selected)
    }
  )

  # Restore graphical parameters
  par(oldpar) 
  
}

# source("gtquery_plot.R")
# #load("data/td.RData")
# .gtdetrend_plot(tdd)
