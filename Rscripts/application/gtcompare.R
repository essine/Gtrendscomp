# Function ".confidencebands_acf" creates confidence data
# for autocorrelations
.confidencebands_acf <- function( data, method )
{# "data"   input data as td$data[-1]
 # "method" smth like options$cbands, "Standard formula" or "Bartlett's formula"
 ntrends = length( data )
 cb = .confidence_band( acf(data[1], plot = FALSE ), method )
 confidence_band <- data.frame( "x" = .confidence_band( acf(data[1], plot = FALSE ), method ) )
 for( i in 2:ntrends) confidence_band = cbind( confidence_band, "x" = .confidence_band( acf(data[i], plot = FALSE ), method ) )
 names(confidence_band) <- names(data)
 return(confidence_band)
}

# Function ".autocorr" creates comparison matrix/matrices
# for autocorrelations
.autocorr   <- function( inputdata, significant_levels ) 
{ # "inputdata" are data like "td$data[-1]"
  # "significant_levels" confidence bands for the "inputdata" ACFs

  l = length(inputdata)
  ac <- matrix( data <- NA, nrow = l, ncol = l)
  rownames(ac) <- colnames(ac) <- names ( inputdata )
  for(i in 1:l) ac[i,i] <- .significant_acorr( acf(inputdata[i], plot = FALSE ), significant_levels[[i]] )
  r <- acorr_sgn <- list() 
  acorr_sgn$title    <- "Significant autocorrelations"
  acorr_sgn$subtitle <- ""
  acorr_sgn$data     <- ac
  r$acorr_sgn <- acorr_sgn
  return(r) 
}

# Function ".crosscorr" creates comparison matrix/matrices
# for crosscorrelations
.crosscorr  <- function( inputdata ) 
{ # "inputdata" are data as "td$data[-1]"
  l = length( inputdata )
  ccorr_lag0 <- matrix( data <- NA, nrow = l, ncol = l )
  rownames( ccorr_lag0 ) <- colnames( ccorr_lag0 ) <- names ( inputdata )
  ccorr_delay <- ccorr_max <- ccorr_lag0
  for( i in 1:l ) for( k in 1:l ) {
    r <- .ccorr( inputdata[i], inputdata[k] )
    ccorr_lag0[i,k]   <- r$ccorr_lag0
    ccorr_max[i,k]    <- r$ccorr_max
    ccorr_delay[i,k]  <- r$ccorr_delay
  }
  ccdelay <- ccmax <- cclag0 <- r <- list() 
  cclag0$title     <- "Crosscorrelation at lag0"
  cclag0$subtitle  <- ""
  cclag0$data      <- ccorr_lag0
  r$ccorr_lag0     <- cclag0
  
  ccmax$title      <- "Crosscorrelation maximum"
  ccmax$subtitle   <- ""
  ccmax$data       <- ccorr_max
  r$ccorr_max      <- ccmax
  
  ccdelay$title    <- "Lag for maximum crosscorrelation"
  ccdelay$subtitle <- ""
  ccdelay$data     <- ccorr_delay
  r$ccorr_delay    <- ccdelay
  
  return( r ) 
}

# Function ".synchrony" creates comparison matrix/matrices
# for synchrony
.synchrony <- function(inputdata) 
{ # "inputdata" are data as "td$data[-1]"
  l = length( inputdata )
  syn <- matrix( data <- NA, nrow = l, ncol = l )
  rownames( syn ) <- colnames( syn ) <- names ( inputdata )
  
  for( i in 1:l ) for( k in 1:l )
    syn[ i, k ] <- .synchrony_xy( inputdata[[i]], inputdata[[k]] )
  
  synch <- r <- list() 
  synch$title    <- "Synchrony"
  synch$subtitle <- ""
  synch$data     <- syn
  r$synchrony    <- synch
  
  return( r ) 
}

# Function ".compare" compares input data and returns comparison data "tc.RData"
.compare <-function (inputdata, subtitle, method, options )
{ # "inputdata"   data to compare ( as td$data )
  # "title"  data info
  # "method" comparison method
  # "options" list of options; options$cbands - confidence bands for the ACF
  tc <- list() 
  switch(method,
     "autocorr"   = { 
        tc$title  <- "Autocorrelation"
        tc$confidence_bands$type <- options$cbands
        tc$confidence_bands$data <- .confidencebands_acf(inputdata[-1], options$cbands)
        tc$comparison   <- .autocorr  (inputdata[-1], tc$confidence_bands$data ) },
     "crosscorr"  = { 
        tc$title  <- "Crosscorrelation"
        tc$comparison   <- .crosscorr (inputdata[-1]) },
     "synchrony" = { 
        tc$title  <- "Synchrony"
        tc$comparison   <- .synchrony(inputdata[-1]) }
  )
  tc$inputdata$title <- "Input data to compare"
  tc$inputdata$subtitle <- subtitle
  tc$inputdata$data <- inputdata
  return(tc)
}

# Function ".gtcompare" compares input data
# and creates comparison data "tc" for further report
.gtcompare <- function( selecteddata, method, options )
{ # "selecteddata" selected data:
  #    "original","model_residuals",
  #    "seasonal","seasonal_trend","seasonal_remainder",
  #    "smoothed","smoothed_remainder","differenced"
  # "method": "autocorr", "crosscorr" and "synchrony"
  # "options": list(cbands);  options$cbands: "Standard formula" or "Bartlett's formula"

  if( missing(selecteddata) ) return("select data to compare please")
  inputdata <- data.frame()
  switch( selecteddata,
     "original" =        { # 'td'
         subtitle <- "(original data)"
         if(is.null(.GlobalEnv$td)) return("no data to compare")
         else inputdata <- .GlobalEnv$td$data },          
     "model_residuals" = { # 'tm'
         subtitle <- paste( "( model residuals for the ", 
         substr(.GlobalEnv$tm$inputdata$subtitle,2,nchar(.GlobalEnv$tm$inputdata$subtitle)) )
         if(is.null(.GlobalEnv$tm)) return("no data to compare")
         else inputdata <- .GlobalEnv$tm$residuals$data },
     "seasonal"        = { # 'tdd'
         subtitle <- "(seasonal components from the seasonal decomposition)"
         if(is.null(.GlobalEnv$tdd)) return("no data to compare")
         else inputdata <- .GlobalEnv$tdd$seasonal$data },
     "seasonal_trend"  = { # 'tdd'
         subtitle <- "(trend components from the seasonal decomposition)"
         if(is.null(.GlobalEnv$tdd)) return("no data to compare")
         else inputdata <- .GlobalEnv$tdd$trend$data },
     "seasonal_remainder" = { # 'tdd'
         subtitle <- "(remainder components from the seasonal decomposition)"
         if(is.null(.GlobalEnv$tdd)) return("no data to compare")
         else inputdata <- .GlobalEnv$tdd$remainder$data },
     "differenced" =      { # 'tdd'
         subtitle <- "(differenced data)"
         if(is.null(.GlobalEnv$tdd)) return("no data to compare")
         else inputdata <- .GlobalEnv$tdd$differenced$data },
     "smoothed"   =      { # 'tdd'
         subtitle <- "(smoothed data)"
         if(is.null(.GlobalEnv$tdd)) return("no data to compare")
         else inputdata <- .GlobalEnv$tdd$smoothed$data },
     "smoothed_remainder" = { # 'tdd'
         subtitle <- "(remainder components from the smoothing)"
         if(is.null(.GlobalEnv$tdd)) return("no data to compare")
         else inputdata <- .GlobalEnv$tdd$remainder$data }
   )
  tc <- .compare( inputdata, subtitle, method, options)
  return(tc)
}

# method : "autocorr", "crosscorr" or "synchrony" 
# source("tcompare.R")
# load("data/tdd.RData")
# tc <- .gtcompare( selectdata = "seasonal_remainder", 
#                  method     = "synchrony" )
