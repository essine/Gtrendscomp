# Function gtcompare_plot plots comparison data
.gtcompare_plot <- function( tc, selectedtraces, selectedacf, selectedccf, selectedcm )
{ # return "error message" or nothing 
  # "tc" comparison data to plot
  # "selectedtraces" traces to plot c(1,3), if missed select all, if c(-1) do not plot traces

  if( missing(tc) ) return("no data to plot")
  if( missing(selectedtraces) ) { n <- length(tc$inputdata$data) - 1
    selectedtraces = c( 1:n ) # select all traces
  }  
  if( missing(selectedacf))  selectedacf = 2 
  else if(selectedacf != -1) selectedacf = selectedacf + 1  # +1 correction while data[1] is 'Date'
  if( missing(selectedccf) ) selectedccf = c( 2, 3 )
  else if(selectedccf[1] != -1) selectedccf = selectedccf + 1  # +1 correction while data[1] is 'Date'
  if( missing(selectedcm)  ) selectedcm  = 1
  
  # save graphical parameters
  oldpar <- par(no.readonly = TRUE)
  
  # Arrange plots
  layout( matrix(c(1,2,3,2), 2, 2, byrow = TRUE) )

  # Plot selected traces
  if( selectedtraces[1] != -1  ) .gtquery_plot( tc$inputdata, selectedtraces)
  else plot.new()

  # Plot comparison data
  switch( tc$title,
    "Autocorrelation" = { 
         .mcplot( m = tc$comparison$acorr_sgn$data, 
                 title = tc$comparison$acorr_sgn$title, 
                 subtitle = "", hide = "ul",
                 number_color = "rb", plotmode = "complex" )
         if(selectedacf != -1){
                 .cfplot( tc$inputdata$data[[selectedacf]],
                          title = paste( "( series:", names( tc$inputdata$data[selectedacf] )," )" ),
                          significant_levels = tc$confidence_bands$data[[selectedacf-1]],                          
                          returns = FALSE ) }
    },
    "Crosscorrelation" = {
         if(selectedcm != -1) {
               m = tc$comparison[[selectedcm]]$data
               title = tc$comparison[[selectedcm]]$title
               .mcplot( m = m, title = title, subtitle = "", hide = "ld",
                        number_color = "rb", plotmode = "complex" )
         }
         if( selectedccf[1] != -1 & length(selectedccf) == 2 ) 
              .cfplot( tc$inputdata$data[[ selectedccf[1] ]], 
                       tc$inputdata$data[[ selectedccf[2] ]],
                       title = paste( "( series:", names( tc$inputdata$data[ selectedccf[1] ]), "and", 
                               names( tc$inputdata$data[ selectedccf[2] ] ), " )" ), 
                       returns = FALSE )
    },
    "Synchrony"     = {

        layout(matrix(c(1,2,1,2), 2, 2, byrow = TRUE))
        if( selectedtraces[1] != -1  ) .gtquery_plot( tc$inputdata, selectedtraces)
        else plot.new()
      
        .mcplot( m = tc$comparison$synchrony$data, 
               title = tc$comparison$synchrony$title, 
               subtitle = "", hide = "ld",
               number_color = "rb", plotmode = "complex" )
    }
  )
  # Restore graphical parameters
  par(oldpar) 
}

# Function ".cfplot" plots auto- or crosscorrelation functions 
.cfplot <- function ( x, y, title, significant_levels, returns )
{  # "x" and "y" are two univariate series
   # "title" adds a subtitle to plot
   # "significant_levels" if significant_levels is missing, the standart level is used, otherwise use "significant_levels" vector
   # "returns": FALSE - no return, TRUE - default

  if( missing(x) & missing(y) ) return ("cfplot: no data to plot")
  else { 
      mode = "ccf"
      if( missing(y) ) { mode = "acf"; inputdata = x  } # acf for x
      if( missing(x) ) { mode = "acf"; inputdata = y  } # acf for y
  }
  if( missing(title) ) stl = "" else stl = title
  if( missing(returns) ) returns = TRUE
  
  if( mode == "ccf" ) { 
     r = ccf( x,y,  plot = FALSE ) 
     tl <- "Crosscorrelation function"
     ylabels <- "CCF" }
  else                { 
     r = acf( inputdata, plot = FALSE ) 
     tl <- "Autocorrelation function"  
     ylabels <- "ACF" }
  
  plot( r$lag, r$acf, xlab = "Lag", ylab = ylabels, ylim = c(-1,1), type ="h")

  l = length(r$lag)
  lines ( c( r$lag[1], r$lag[l] ), c(0,0) ) # middle line

  # Significant levels
  if(missing(significant_levels)) { # ablines
    sl = qnorm((1 + 0.95)/2)/sqrt( r$n.used ) # "sl" significant level        
    lines ( c( r$lag[1], r$lag[l] ), c(sl,sl), col = "blue", lty = "dashed" ) # up line
    lines ( c( r$lag[1], r$lag[l] ), c(-sl,-sl), col = "blue", lty = "dashed") # down line
  }
  else { # plot significant_levels
    for(i in 2:l)  {
       lines ( c( r$lag[i-1], r$lag[i] ), c(significant_levels[i-1],significant_levels[i]), col = "blue", lty = "dashed") # up line 
       lines ( c( r$lag[i-1], r$lag[i] ), c(-significant_levels[i-1],-significant_levels[i]), col = "blue", lty = "dashed") # down line 
    }
  }
  
  # Title and subtitle
  mtext( tl,  line=2, cex=1.1) # title
  mtext( stl, line=1, cex=1)   # subtitle
  
  if(returns) return(r)
}
# f = c(1,2,3,4,4,5,67,8,9)
# r = .cfplot(f, title = "series f")
# r = .cfplot(x = f, y = f, title = "series f")

#    source("gtquery_plot.R")
#    source("mcplot.R")
#   # #load("data/tc.RData")
#    .gtcompare_plot(tc)
