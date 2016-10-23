# Function "detrend" returns detrended y(t)
.detrend <- function(t,y,method)
{ # "t" date, "y" data, "method" detrending method
  switch( method,
         "season" = { # seasonal decomposition
           dl=1/52; d=format(t[1],"%Y %m %d")
           st=c(as.numeric(substr(d,1,4)),as.numeric(substr(d,6,7)))
           s=stl(ts(y, start=st, deltat = dl), s.window="per")
           dd=cbind(as.vector(s$time.series[,1]),  # seasonal
                    as.vector(s$time.series[,2]),  # trend
                    as.vector(s$time.series[,3]) ) # remainder
         },
         "smooth" = { # smoothing
           s=loess.smooth( as.Date(t),y,evaluation = length(t) )
           dd=cbind(s$y, y - s$y) # smoothed and remainder  
         },
         "diff"   = { # differencing
           dd=diff(y) # diff length -1
         }
  ) 
  return(dd)
}

# Function ".gtdetrend" creates and returns "tdd" detrended data,
# can return an error string 
.gtdetrend <- function(td, method) 
{ # "td" data, 
  # "method" detrending method: 
  #         "season" - seasonal decomposition,
  #         "smooth" - smoothing,
  #         "diff"   - differencing.
  # "tdd" - detrended data to return
  
  if(missing(td))     return("no data to detrend")
  if(missing(method)) return("no method to detrend")
  else if(! method %in% c("season","smooth","diff") ) 
    return("method shold be 'season', 'smooth' or 'diff'")
  
  # Create "sn", "sm" or "df"
  dd <- .detrend(td$data[,1],td$data[,2],method)
  switch(method,
    "season" = { 
       sn <- list(title = "Seasonal decomposition")
       
       sn$seasonal$title    <- "Seasonal components"
       sn$seasonal$subtitle <- ""
       sn$seasonal$data     <- data.frame(td$data[,1],dd[,1])
       
       sn$trend$title    <- "Trend components"
       sn$trend$subtitle <- ""
       sn$trend$data     <- data.frame(td$data[,1],dd[,2])
       
       sn$remainder$title    <- "Remainder components"
       sn$remainder$subtitle <- ""
       sn$remainder$data     <- data.frame(td$data[,1],dd[,3])
    },
    "smooth" = { 
       sm <- list(title = "Smoothing")
       
       sm$smoothed$title    <- "Smoothed components"
       sm$smoothed$subtitle <- ""
       sm$smoothed$data     <- data.frame(td$data[,1],dd[,1]) # smoothed
       
       sm$remainder$title   <- "Remainder components"
       sm$remainder$subtitle<- ""
       sm$remainder$data    <- data.frame(td$data[,1],dd[,2]) # smoothed
    },
    "diff"   = { 
       df <- list(title="Differencing")
       
       df$differenced$title    <- "Differenced traces"
       df$differenced$subtitle <- ""
       l <- length(td$data[,1])
       df$differenced$data <- data.frame(td$data[,1][-l],dd) # differenced
    })
  
  # Fill "sn", "sm" and "df" data frames
  l <- length(td$data)
  for(i in 3:l) { 
    dd=.detrend(td$data[,1],td$data[,i],method)
    switch( method,
      "season" = { 
        sn$seasonal$data    = cbind(sn$seasonal$data,  dd[,1]) # seasonal
        sn$trend$data       = cbind(sn$trend$data,     dd[,2]) # trend
        sn$remainder$data   = cbind(sn$remainder$data, dd[,3]) # remainder
      },
      "smooth" = { 
        sm$smoothed$data    = cbind(sm$smoothed$data,  dd[,1]) # smoothed
        sm$remainder$data   = cbind(sm$remainder$data, dd[,2]) # remainder
      },
      "diff"   = { 
        df$differenced$data = cbind(df$differenced$data,dd)    # differenced
      }
    ) # switch end      
  } # for end
  
# Correct columns names and create tdd
  tdd <- list()
  switch( method,
      "season" = { 
        names(sn$seasonal$data)    <- names(sn$trend$data) <- names(sn$remainder$data) <- names(td$data) 
        tdd<-sn
      },
      "smooth" = { 
        names(sm$smoothed$data)    <- names(sm$remainder$data) <- names(td$data); 
        tdd<-sm 
      },
      "diff"   = { 
        names(df$differenced$data) <- names(td$data)
        tdd<-df 
      }
  )
  tdd$original <- td  # add original traces
  tdd$original$title <- "Non-detrended data"
  tdd$original$subtitle <- ""
  
  # Return data
  return(tdd)
}
#   load("data/td.RData")
#   #tdd <- .gtdetrend(td, method ="season")
#   #tdd <- .gtdetrend(td, method ="smooth")
#   tdd  <- .gtdetrend(td, method ="diff")
#   save(tdd,file="data/tdd.RData")
