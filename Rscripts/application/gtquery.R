# Function ".call_gtrendsR" calls gtrends() fuction from 'gtrendsR' library
# to retrieve and return trends data. 
.call_gtrendsR <- function( lsearch, daterange, options ) 
{ # "lsearch" - search list, c("Search1","Search2",...)
  # "daterange" date range = c("start_date","end_date")
  # "options" : list(timescale = "NA",geo = c("all"), category = "0")

  # Retrieve data 
  trendsdata = "Google data retrieval failed."
  if(options$timescale == "NA") { # data range is active
     if( options$geo[1] == "all" ) 
         trendsdata <- try( gtrends( query = lsearch, cat = options$category, start_date=as.Date(daterange[1]), end_date=as.Date(daterange[2])) )
     else 
         trendsdata <- try( gtrends( query = lsearch, geo = options$geo, cat = options$category, start_date=as.Date(daterange[1]), end_date=as.Date(daterange[2])) )
     if( is.character(trendsdata) ) return( paste("Google data retrieval failed. gtrends():",trendsdata ) )  # data retrieval failed
     TM <- cbind( Date = as.Date(trendsdata$trend$end), trendsdata$trend[c(-1,-2)] )
  }
  else {
     if( options$geo[1] == c("all") ) 
         trendsdata <- try( gtrends( query = lsearch, cat = options$category, res = options$timescale ) )
     else 
         trendsdata <- try( gtrends( query = lsearch, geo = options$geo, cat = options$category, res = options$timescale ) )
     if( is.character(trendsdata) ) return( paste("Google data retrieval failed. gtrends():",trendsdata ) )  # data retrieval failed
     TM <- cbind( Date = as.Date(as.POSIXct(trendsdata$trend$start)), trendsdata$trend[-1] )
  }
  return(TM)
}

# Function ".request_data" retrieves and returns trends data
# according to the "datatype" parameter
.request_data <- function( lsearch, daterange, options, datatype ) 
{ # "lsearch" - search list, c("Search1","Search2",...)
  # "daterange" date range = c("start_date","end_date")
  # "options" : list(timescale = "NA",geo = c("all"), category = "0")
  # "datatype" - 'google_original' or 'equally_scaled'
  
  swn=length(lsearch)
  switch( datatype,
   "google_original"    = { 
      TM <- .call_gtrendsR( lsearch, daterange, options )
      if( is.character(TM) ) return(TM) # .call_gtrendsR() returns an error message
   },
   "equally_scaled"     = { 
      TM <- .call_gtrendsR( lsearch[1], daterange, options ) # 1st search item
      if( is.character(TM) ) return(TM) # .call_gtrendsR() returns an error message
      for(i in 2:swn) { 
         trend <- .call_gtrendsR( lsearch[i], daterange, options )
         TM = cbind(TM,trend[[2]]) }
   }
  )
  return( na.omit(TM) ) # exclude NA
}

# Function ".rescale_trends" creates and returns "improved_precision" data
.rescale_gtrends <- function( TMg, TMe )
{ # "TMg" - "google_original" trends data; "TMe" - "equally_scaled" trends data
  
  # calculate trends relations from TMg data
  trends_max    = apply(X = TMg[-1], MARGIN = 2, FUN = max)
  trends_scale  = 100.0/trends_max
  
  # correct inf data in "trends_scale", div to zero
  trends_number = length (trends_max)
  for( i in 1:trends_number ) if( is.infinite(trends_scale[i]) ) trends_scale[i] <- 1.0

  # rescale TMe data
  TM = scale(as.matrix(TMe[-1]), center = FALSE, scale = trends_scale )
  TM = cbind(Date=TMg[1],TM)
  return(TM)
}

# Function ".gtquery" retrieves and returns "td" trends data;
# can return an error string; requires library 'gtrendsR'.

.gtquery <- function(usr, pwd, lsearch, daterange, timescale, geo, category, datatype) 
{ # "usr" - login, "pwd" -password
  # "lsearch" - search list, c("Search1","Search2",...)
  # "daterange" date range = c("start_date","end_date")
  # "timescale" time scale; "NA", "1h","4h","1d"or "7d"; default "NA" - the "datarange" is active
  # "geo" geolocation; character string, e.g. "US-AL,CA" USA Alabama State and Canada; will be converted to c("US-AL","CA"); default "all"
  # "category" category code; e.g. "0-18" Shopping, "0-7" Finance, "0-5-32" Software; default "0"
  # "datatype" - 'google_original', 'equally_scaled' or 'improved_precision' 

  # Check parameters
  if(missing(usr))       return("user information is missed")
  if(missing(pwd))       return("password information is missed")
  
  if(missing(lsearch))   return("search information is missed")
  l=length(lsearch)
  if(l<2)                return("needs at least two trends to compaire later")
  for(i in 1:l) if( is.null(lsearch[i]) | nchar(lsearch[i])==0 ) 
  return("a search field is selected but not filled out")

  if(missing(daterange)) 
    daterange <- c( .sdate, as.Date(Sys.time()) ) 
  else {
    if(daterange[2] <= daterange[1]) return("The date range is invalid. The start date must be later than the end date.")
    if(daterange[1] < .sdate) return( paste("The date range is invalid. The start date must be later than", .sdate) )
  }
  
  # Check/set parameters: options "timescale", "geo" and "category"
   options <- list()

  if(missing(timescale)) options$timescale <- "NA"
  else options$timescale <- timescale

  if(missing(geo)) options$geo <- c("all")
  else options$geo <- unlist(strsplit(geo, "[,]"))

  if(missing(category)) options$category <- "0"
  else options$category <- category
  
  if(missing(datatype))      datatype <- 'google_original'

  # Connect to a google account 
  # library(gtrendsR) # library load is placed in "global.R"
  ch = "Google login failed"
  ch <- try( gconnect(usr, pwd), silent = FALSE )       # connect
  if( is.character(ch) ) return( paste("Google login failed. gconnect():",ch) )  # connection failed

  # Retrieve data
  switch( datatype,
   "google_original"    = { subtitle = "Google trends data." 
      TM <-  .request_data( lsearch, daterange, options, datatype )
      if( is.character(TM) ) return(TM) # .request_data() returns an error message
   },
   "equally_scaled"     = { subtitle = "Equally scaled google trends." 
      TM <-  .request_data( lsearch, daterange, options, datatype)
      if( is.character(TM) ) return(TM) # .request_data() returns an error message
   },
   "improved_precision" = { subtitle = "Google trends data of improved precision." 
      TMg <- .request_data( lsearch, daterange, options, "google_original")
      if( is.character(TMg) ) return(TMg) # .request_data() returns an error message

      TMe <- .request_data( lsearch, daterange, options, "equally_scaled")
      if( is.character(TMe) ) return(TMe) # .request_data() returns an error message

      TM  <- .rescale_gtrends(TMg,TMe)
   }
  )

  # set column names, cut column names to "max_cnl" chars
  max_cnl = .GlobalEnv$.cutquerysize; names(TM)<-c("Date",substr(lsearch,1,max_cnl))

  # subtitle
  subtitle = paste(subtitle, paste(" Query: ",    toString(lsearch), ".", sep=""), 
                             paste(" Geo: ",      toString(geo),     ".", sep=""), 
                             paste(" Category: ", category,          ".", sep="") )
  # return data 
  title="Interest over time"
  td=list(title     = title, subtitle=subtitle,
          query     = lsearch,
          geo       = geo,
          category  = category,
          daterange = daterange,
          timescale = timescale,
          datatype  = datatype,
          data      = TM )
  return(td)
}

# library(gtrendsR)
# usr_value <- "usr"; pwd_value <- "pwd"
# td<-.gtquery ( usr = usr_value, pwd = pwd_value, 
#                 lsearch=c("SAS", "SPSS"),
#                 daterange=c("2011-03-01","2016-03-01"), 
#                 datatype = "google_original" )