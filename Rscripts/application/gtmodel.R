# Function ".lbtest" performs Ljung-Box (LB) test for
# model residuals and save analysis data in "tm",
# requires Box.test() from "stats" library 
.lbtest <- function( inputdata )
{
  # Set lag to 10 for non-seasonal data
  # http://robjhyndman.com/hyndsight/ljung-box-test/
  if( length(inputdata$data[,1]) > 10 ) lag=10
  else  lag = length(inputdata$data[,1]) - 1
  
  # Test for autocorrelation
  lb <- list()
  lb$title <- "Ljung-Box tests for model residuals"
  for(i in 2:length(inputdata$data)) {
    s <- Box.test(inputdata$data[,i], lag = lag, type = "Ljung-Box",fitdf = 0)
    lb$p.value[[i-1]]  <- s$p.value
    lb$Xsquared[[i-1]] <- s$statistic
    lb$df[[i-1]]       <- s$parameter
  }
  
  # Set names and return
  names(lb$p.value) <- names(lb$df) <- names(lb$Xsquared) <- names(inputdata$data[-1])
  
  return(lb)
}  

# Function ".model" models input data and returns model data
# requires auto.arima() from "forecast" library 
.model <- function( inputdata, title )
{ # "inputdata" data to model : 'Date','Trace1','Trace2',...
  time <- inputdata[1]; traces <- inputdata[-1] 
  tm <- list(); tm$title <- paste("ARIMA model for the", title)
  
  tm$inputdata$title <- "Input data to model"
  tm$inputdata$subtitle <- paste("(",title,")")
  tm$inputdata$data <- inputdata
  
  tm$residuals$title <- "ARIMA model residuals"
  tm$residuals$subtitle <- ""
  tm$residuals$data <- data.frame(names(inputdata)[1] <- time)
  
  for( i in 1:length(traces) ) {
    # restrict auto.arima to non-seasonal models and limited number of coefficients
    aa <- auto.arima( traces[i] , max.p = 5, max.q = 5, seasonal = FALSE) 

    # model residuals
    tm$residuals$data[[ names(traces)[i] ]] <- as.vector(aa$residuals)
    
    # model parameters
    p <- aa$arma[1]; q <- aa$arma[2]; d <- aa$arma[6]
    tm$pdq[[ names(traces)[i] ]] <- c( p, d, q ) 

    # model coefficients
    nr <- max(p,q); if(nr == 0) nr = 1
    mc <- matrix(data <- 0, nrow = nr, ncol = 4)
    names(mc) <- c("ar","var","ma","var")

    for(k in 1:p ) { if(p!=0) { # ar1, ar2, ...
      mc[k,1] <- c(aa$model$phi[k])
      mc[k,2] <- aa$var.coef[k,k]  }}

    for(k in 1:q ) { if(q!=0) { # ma1, ma2, ...
      mc[k,3] <- c(aa$model$theta[k])
      mc[k,4] <- aa$var.coef[k+p,k+p]  }}

    tm$coefficients[[ names(traces)[i] ]] <-  mc
    
    # drift, mean (intercept) if exist
    l <- length(aa$coef)        
    if( l > ( p + q ) ) 
      tm$constant_var[[ names(traces)[i] ]] <- c( aa$coef[l], aa$var.coef[l,l] )
    else
      tm$constant_var[[ names(traces)[i] ]] <- c( NA, NA)
    
    # model sigma^2, likelihood, AIC, AICc, BIC
    tm$sigma2[[ names(traces)[i] ]] <-  aa$sigma2
    tm$loglik[[ names(traces)[i] ]] <-  aa$loglik
    tm$aic   [[ names(traces)[i] ]] <-  aa$aic
    tm$aicc  [[ names(traces)[i] ]] <-  aa$aicc
    tm$bic   [[ names(traces)[i] ]] <-  aa$bic
  }
  return(tm)
}

# Function ".gtmodel" models input data
# and creates model data "tm" for further comparison (residuals)
.gtmodel <- function( inputdata, selectdata )
{ # "inputdata" data (td or tdd), "selectdata" selected data:
  # "original","seasonal_remainder","differenced" or "smoothed_remainder"
  if( missing(inputdata) )  return("no data to model")
  
  if( is.null(inputdata$original) ) selectdata <- "td" # 'td' instead of 'tdd'
  else if( missing(selectdata) ) return("select data to model please")

  switch( selectdata,
    "td"                 = { tm <- .model( inputdata$data, "original data") },
    "original"           = { tm <- .model( inputdata$original$data, "original data") },
    "seasonal_remainder" = { tm <- .model( inputdata$remainder$data, "remainder components from the seasonal decomposition" ) },
    "differenced"        = { tm <- .model( inputdata$differenced$data, "differenced data") },
    "smoothed_remainder" = { tm <- .model( inputdata$remainder$data, "remainder components from the smoothing") }
  )

  tm$lb_test <- .lbtest( tm$residuals ) # perform LB test
  return(tm)
}

#load("data/td.RData")
#tm <- .gtmodel( inputdata = tdd, selectdata="seasonal_remainder" )
#r<-tm$residuals
#tm <- .gtmodel( inputdata = r)
#library("forecast")
#tm <- .gtmodel(inputdata=td)
#save(tm,file="tm.RData")
#tm <- .gtmodel(tdd, "differenced")