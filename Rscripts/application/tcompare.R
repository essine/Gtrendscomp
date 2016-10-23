# Functions to calculate traces comparison data:
# .confidence_band(), .significant_acorr(), .ccorr(), .synchrony() and .bdiff()

# Function ".confidence_band" calculates confidence bands for a ACF and
# returns confidence bands data as a vector
.confidence_band <- function( acfdata, method ) 
{ # "acfdata" data returned by acf()
  # "method" smth like options$cbands, "Standard formula" or "Bartlett's formula"
  
  nlags = length( acfdata$acf )
  significant_levels <- c(1:nlags)
  if(method == "Bartlett's formula") { # "Bartlett's formula"
    for(k in 1:nlags ) { s = 0; for(i in 1:k) s = s + acfdata$acf[i] * acfdata$acf[i]  
      significant_levels[k] = qnorm((1 + 0.95)/2)*sqrt( ( 1 + 2*s)/acfdata$n.used )  } 
  } 
  else { # "Standard formula"
    for(k in 1:nlags )  
      significant_levels[k] = qnorm((1 + 0.95)/2)/sqrt( acfdata$n.used ) #confidence bands have fixed width that depends on the sample size
  }
  return(significant_levels)
}

# Function ".significant_acorr" calculates numbers of significant autocorrelations for "acfdata"
.significant_acorr <- function( acfdata, significant_levels ) 
{ # "acfdata" data returned by acf()
  # "significant_levels" confidence bands for the "acfdata"

  l <- length( acfdata$acf )
  number_of_significant = -1
  for( i in 1:l ) if( acfdata$acf[i] > significant_levels[i] | acfdata$acf[i] < -significant_levels[i] ) number_of_significant = number_of_significant + 1
  return( number_of_significant )
}

# Function ".ccorr" calculates crosscorrelation parameters for traces x and y
# returns ccorr_lag0, ccorr_max, ccorr_delay
# x and y should be of of equal length
.ccorr <- function( x, y )
{
  r <- list()
  res = ccf( x, y, plot=FALSE ) 
  
  l <- length(res$acf)
  ccmax <- res$acf[1];  lagmax <- 1
  for(i in 1:l ) if( res$acf[i] > ccmax ) { ccmax = res$acf[i]; lagmax = i } 

  lag0 = (l-1)/2+1
  r$ccorr_lag0  = res$acf[lag0]   # ccorr at lag0
  r$ccorr_max   = res$acf[lagmax] # maximal ccorr 
  r$ccorr_delay = lagmax - lag0   # delay
  return(r)
}

# Function ".synchrony_xy" calculates and returns synchrony of changes in curves directions
# for two vectors x and y; calls .bdiff() function
.synchrony_xy <- function( x, y ) 
{ # x and y should be of of equal length
    bdx = .bdiff( x );  bdy = .bdiff( y )
    
    bdn = length( bdx ); syn = 0; negs = 0
    for( i in 1:bdn ) { 
      a = abs( bdy[i] - bdx[i] )
      if( a == 0 ) syn  = syn + 1  # synchrony case
      if( a == 2 ) negs = negs + 1 # negative synchrony case 
    }
    sn = ( syn - negs ) / bdn
    
    return( sn )
}
#.synchrony_xy(tc$inputdata$data$RStudio, tc$inputdata$data$SAS)

# Function "bdiff" calculates changes in vector "y" and returns
# vector of values -1,0,1 (negative,no or positive changes)
# Note that the returned value is a vector which is shorter than "y" 
.bdiff <- function( y )
{# "y" numeric vector, vector size should be > 2
    if(missing(y))        { print("Error: Function 'bitdiff': no input vector"); return(-1) }
    else if(length(y)<2)  { print("Error: Function 'bitdiff': vector size should be > 2"); return(-1) }
  
    bdn = length(y)-1; bd = c(1:bdn)
    for( i in 1:bdn ) { 
       yd = y[i+1] - y[i]
       if( yd == 0 ) bd[i] = 0          # no change 
       else { if( yd > 0 ) bd[i] = 1    # positive change
              else         bd[i] = -1 } # negative change 
    }
    
    return(bd)
}