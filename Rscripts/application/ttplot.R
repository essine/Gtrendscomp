# "ttplot.R" - text table plot functions:
# .table_plot() and .column_width(), .matrix_width()
# .arima_plot() and .lbtest_plot()

# Function ".matrix_width" calculates matrix width
.matrix_width <- function( textmatrix, property )
{
  l = nrow( textmatrix )
  if(missing(property)) property <- 
      matrix( data <- NA, ncol = ncol(textmatrix), nrow = l)
  w <- 0
  for(i in 1:l) {
    w = w + .column_width( textmatrix[l,], property[l,] )
  }
  otherpar <- par()
  return( otherpar$cxy[1] * w )
}

# Function ".column_width" 
.column_width <- function( textcolumn, propertycolumn )
{ # calculates column width for 'textcolumn'
  # excluding lines ("l") based on 'propertycolumn'
  l = length(textcolumn); max = 1

  for(k in 1:l) { line = FALSE
  
    if(!is.na(propertycolumn[k])) # property exist
      if( length( grep("l",propertycolumn[k]) ) ) # property is line 
        line = TRUE        
    
    if(line == FALSE)  
    {  # not line
      n <- nchar( textcolumn[k] )
      if( n > max ) max = n
    }  
  }
 return( max )
}  

# Function ".table_plot" plots "textmatrix"
.table_plot <- function( textmatrix, expressions, property, title, marxy )
{ # "textmatrix" matrix with text to plot
  # "expressions" list of expressions, is used in case property contains "e"
  # to address "expressions" use a name stored in "textmatrix" expressions[[textmatrix[i,k]]]
  # "property" "l" line, "e" expression, "le" line & expression
  # "title" title
  # "marxy" margins for table (0,1)
  
  # check parameters
   if(missing(property)) property <- 
       matrix( data <- NA, ncol = ncol(textmatrix), nrow = nrow(textmatrix) )
   if(missing(expressions)) exressions <- NA
   if(missing(marxy)) marxy <- c(0,0)
   if(missing(title)) title <- ""
    
  # save graphical parameters
  #oldpar <- par(no.readonly = TRUE)
  otherpar <- par() # otherpar$cxy[1] - character width
  # otherpar$cxy[2] - character height in xy units
  dx = otherpar$cxy[1]; dy = otherpar$cxy[2]*1.5
  
  #plot.new()
  #par(mar=c(0,0,0,0))
  #plot.window(xlim = c(0, 1), ylim = c(0,1))
  
  # Calculate shifts
  nlines = nrow(textmatrix)
  ncolumns = ncol(textmatrix)
  # sx columns shift
  sx <- c(); sx[1] <- marxy[1]
  for(k in 1:ncolumns){
    sx[k+1] <- sx[k] + dx * .column_width(textmatrix[,k],property[,k])
  } 
  # dy rows shift
  sy = c(); sy[1] <- 1 - dy - marxy[2]
  for(i in 1:nlines){
    sy[i+1] <- sy[i] - dy 
  } 
  
  # Plot title
  text( sx[1], sy[1]+dy, adj=0, title, font = 2, cex = 1.1)
  
  # Plot text and grids
  for(i in 1:nlines){
    for(k in 1:ncolumns) {
      x = sx[k]; y = sy[i]
      
      if(i == 1) { density = 100; col = "lightgrey"; myfont=2 } # Title in table
      else       { density = NULL; col = NULL; myfont = 1 }

      
      if( is.na( property[i,k] ) ) {
        rect( sx[k]-dx/2, sy[i]+dy/2, sx[k+1]-dx/2,sy[i+1]+dy/2,
              border="black", density = density, col = col )
        
        text( x, y, adj=0, textmatrix[i,k], font = myfont)
      }
      else { # if line "l" or expression "e" or smth else
        mytext <- textmatrix[i,k]; myborder = FALSE
        if(length(grep("e",property[i,k]))) { # expression
          mytext <- expressions[[ textmatrix[i,k] ]]
          myborder = TRUE
        }
        if(length(grep("l",property[i,k]))) { # line
          if(k!=1) mytext <- "" # except of fist column
          myborder = FALSE
        }  
        text( x, y, adj=0, mytext )
        if(myborder) { # draw border
          rect( sx[k]-dx/2, sy[i]+dy/2, sx[k+1]-dx/2,sy[i+1]+dy/2,
                border="black", density = density, col = col )
        }  
      }
    }
  }
  
  # Restore graphical parameters
  # par(oldpar)
}

# Function .arima_plot plots arima model parameters
.arima_plot <- function(tm, selected){
# 'selected' 1,2 or 3 - selected trace to plot coefficients
# 'tm' data to plot

  plot.new()
  par(mar=c(0,0,0,0))
  plot.window(xlim = c(0, 1), ylim = c(0,1))

  # Arima p,d,q
  nr <- length(tm$pdq) + 1
  textmatrix1 <- matrix( nrow = nr, ncol = 2 )
  textmatrix1[1,] <- c("Trace","p,d,q") # title
  textmatrix1[,1][-1] <- names(tm$pdq)
  for(i in 2:nr)
    textmatrix1[i,2] <- paste(tm$pdq[[i-1]][1],",",tm$pdq[[i-1]][2],",",tm$pdq[[i-1]][3],sep="")
  
  # ARIMA Coefficients
  nr = nrow(tm$coefficients[[selected]]) + 1
  property <- textmatrix2 <- matrix( data <- NA, nrow = (nr+3), ncol = 4 )
  textmatrix2[1,] <- c("AR ","var.c ","MA ","var.c " ) # title
  for(i in 2:nr ) for(k in 1:4 ) {
    if(tm$coefficients[[selected]][i-1,k] == 0) textmatrix2[i,k] <- "" 
    else textmatrix2[i,k] <- format(tm$coefficients[[selected]][i-1,k], digits=2, nsmall=2 ) 
  }
  
  if( !is.na(tm$constant_var[[selected]][1]) ){
    t <- paste( names(tm$constant_var[[selected]])[1],format(tm$constant_var[[selected]][1], digits=2, nsmall=2 ),
                " var.coef ", format(tm$constant_var[[selected]][2], digits=2, nsmall=2 ) )
  } else { t <- "" }
  textmatrix2[nr+1,1] <- t; property [nr+1,] <- "l"
  
  # expression sigma2
  t <- paste(              "= ", format(tm$sigma2[[selected]], digits=2, nsmall=2 ) , 
       "     log likelihood = ", format(tm$loglik[[selected]], digits=2, nsmall=2 ))
  expressions <- list()
  expressions[["sigma2"]] <- bquote( sigma^2~.(t) ) 
  textmatrix2[nr+2,1] <- "sigma2"; property [nr+2,] <- "le" 
  
  t <- paste("AIC =",  format (tm$aic [[selected]], digits=2, nsmall=2 ),
             " AICc =",format (tm$aicc[[selected]], digits=2, nsmall=2 ),
             " BIC =", format (tm$bic [[selected]], digits=2, nsmall=2 ))
  textmatrix2[nr+3,1] <- t; property [nr+3,] <- "l" 
  
  title = paste("ARIMA coefficients for '",names(tm$coefficients)[selected],"'",sep="")
  .table_plot(textmatrix = textmatrix2, title = title, property = property, marxy = c(0.1,0.1), expressions =  expressions)
  .table_plot(textmatrix = textmatrix1, title = "ARIMA(p,d,q)", marxy = c(0.7,0.1) )
}

# Function .lbtest_plot plots LB test data
.lbtest_plot <- function(testdata)
{ # "testdata" as tm$lb_test
  plot.new()
  par(mar=c(0,0,0,0))
  plot.window(xlim = c(0, 1), ylim = c(0,1))
  
  title <- testdata$title
  nr <- length(testdata$p.value) + 1
  textmatrix <- matrix( data <- "", nrow = nr, ncol = 4 )
  
  textmatrix[1,] <- c("Trace","p value","X-squared","df ") # title
  textmatrix[,1][-1] <- names(testdata$p.value)
  
  # p values
  textmatrix[,2][-1] <- format (testdata$p.value, digits=2, nsmall=2 )
  # X-square
  textmatrix[,3][-1] <- format (testdata$Xsquared, digits=2, nsmall=2 )
  # df
  textmatrix[,4][-1] <- paste(testdata$df)
  
  # calculate margins and plot title
  x <- (1-.matrix_width(textmatrix))/2
  #text( x, 0.9, adj=0, title, font = 2, cex = 1.2) # title
  
  .table_plot(textmatrix = textmatrix, title = title, marxy = c(x,0.1) )  
}

#.arima_plot(tm = tm, selected = 1)
#.lbtest_plot(tm$lb_test)
