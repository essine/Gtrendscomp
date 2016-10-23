# Function select_colors returns 
# "colors" for selected traces
# "selected" c(1,3) - select traces 1 and 3
.select_colors <- function(selected) {
  colors <- sapply(selected,function(x) x + 1)
  return(colors)  
}

# Function select_traces returns 
# selected traces data to plot -> "trs"
# "traces" traces data frame as "td$data"
# "selected" c(1,3) - select traces 1 and 3
.select_traces <- function(traces, selected) {
  
  ns = length(selected)   # number of selected traces
  nt = length(traces) - 1 # number of all traces
  
  trs <- data.frame(traces[,1]) # date
  names(trs)[1] <- names(traces)[1]

  for(i in 1:ns) { # selected
    for(k in 1:nt) { # all traces, out of date
      if( selected[i] == k ) {
        trs <- cbind( trs, traces[,k+1] )
        names(trs)[i+1] <- names(traces)[k+1]
      }
    }
  }

  return(trs)  
}

# Function select_ylimits returns ylim c(min,max)
.select_ylimits <- function ( trs, ylimits)
{
  if( ylimits[3] == 0 ) { # absolute values of ylimits
    return( c(ylimits[1], ylimits[2]) )
  }
  else { # values of ylimits * min and max of "trs"
    return( c(ylimits[1]*min(trs[-1]), ylimits[2]*max(trs[-1])) )
  }
}  

# Function plot_traces plot selected traces
.plot_traces <- function(traces, colors, title, subtitle, ylim) {
# "traces" traces data frame as "td$data"
  nt     <- length(traces)          # number of traces plus date
  labels <- colnames( traces[-1] )
  time   <- traces[ ,1]             # date
  
  plot( time, traces[ ,2], type="l" , col = colors[1], 
        xlab="Date", ylim = ylim, ylab="Search hits")
  
  if( nt > 2 ) { # more than one trace to plot
    for( i in 3:nt ) lines( time, traces[ ,i], 
                            col = colors[i-1] )
  }
  
  # Legends  
  legend( "bottomright", legend = names(traces)[-1], 
          text.col = colors, bty="n", cex=1 )
  
  # Title and subtitle
  mtext( title,    line=2, cex=1.1)
  mtext( subtitle, line=1, cex=1)
}  

# Function .gtquery_plot plots selected traces
# and returns "error message" or nothing
.gtquery_plot <- function(td, selected, ylimits )
{ 
  # "td" trends data, title, subtitle
  # "selected" traces to plot c(1,3), if missed select all
  # "ylimits" y axis limits c( min, max, mode ), if missed use default 
  #   if mode == 0, absolute values of min and max will be used  
  #   if mode == 1, min and max are factors for minimal and maximal values
  
  # Select traces and colors
  if( missing(selected) ) { n <- (length(td$data) - 1)
    selected <- c( 1:n ) # select all traces
  }  
  if(is.null(selected))   return("No traces are selected")

  trs    <- .select_traces( td$data, selected )
  colors <- .select_colors( selected )
  
  # Select ylimits
  if(missing(ylimits)) ylimits <- c( 1, 1, 1 )
  if(length(ylimits) != 3) return("ylimits size should be 3")
  ylim <- .select_ylimits ( trs, ylimits)

  # Plot traces
  .plot_traces( trs, colors, td$title, td$subtitle, ylim )  
}

#load("data/td.RData")
#.gtquery_plot( td, selected = c(1,3) )
#.gtquery_plot( td, selected = c(1,3), ylimits = c(1,1.2,1) )
