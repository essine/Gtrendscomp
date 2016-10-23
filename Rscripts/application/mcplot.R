# Function "setcol" set color for a matrix cell
.setcol <- function( value, mode )
{ # "value" matrix cell value, "mode": "b" or "rb"
  color = 1 # black by default
  #if( mode == "b" ) color = 1  # black color
  if( mode == "rb") {
    if( value < 0 ) color = 2  # red
    if( value > 0 ) color = 4  # blue
  }
  return(color)
}

# Function "mark" plot a circle within the matrix cell
.mark <- function( x, y, r, mv, tm )
{# tm[1] - tm[2] range, 
 # tm[3] mode ("0" - mark if in the range, "1" if out of the range)
 # "x","y" circle position, "r" - radius
 # "mv" meaning to compare with tm[1] and tm[2]
  
  if(mv==tm[1] | mv==tm[2]) return() # on the range border
  if(tm[3]==0) # mark if in the range
    if(mv<tm[1] | mv>tm[2]) return() # out of the range
  if(tm[3]==1) # mark if out of the range
    if(mv>tm[1] & mv<tm[2]) return() # in the range

  r = r*0.95
  x1 = cos( seq( 0, 2*pi, pi/180 ) ) * r + x
  y1 = sin( seq( 0, 2*pi, pi/180 ) ) * r + y 
  lines( x1,y1, col = "darkgrey" )
  polygon( x1, y1, col = "grey",  density = 100 )
}

# Function "blank" plots empty plot
.blank <- function( title )
{
  oldpar <- par( no.readonly = TRUE )
  par( mar = c(0,0,0,0) );  plot.new()           
  plot.window( xlim = c(0, 1), ylim = c(0,1), asp = 1 )
  
  rect( 0, 0, 1, 0.9, border = "red" )
  lines( x = c(0,1,1,0), y=c(0,0.9,0,0.9), col="red" )
  text( x = 0.5, y = 1, title, cex = 1.3, font = 2 )
  
  par(oldpar)
}

# Function "mcplot" plots comparison matrix "m"
.mcplot <- function( m, title, subtitle, hide, 
                     label_color, number_color,threshold, plotmode )
{ # "m" - square matrix, other args are optional
  # "title" - titLe, 
  # "subtitle" - subtitle,
  # "hide" - hide "u" the upper part of matrix , "l" the lower part of matrix, "d" diagonal, 
  #        "ud" upper and diagonal parts, "ld" lower and diagonal parts, if "hide" parameter is missed, nothing to hide.
  # "label_color" - colors for labels c(1,2,3,...)
  # "number_color" - colors for numbers: "rb" red/blue, "b" black (default)
  # "threshold" - Threshold levels to Mark, c(min value, max value)
  # "plotmode"  - "alone","complex" if is a part of complex plot
  
  # Check, set parameters
  if(missing(m) | !is.matrix(m)){ .blank(".mcplot: no data"); return() }
  if(missing(title)) title = ""
  if(missing(subtitle)) subtitle =""
  if(missing(label_color)) label_color = c( 1:ncol(m) + 1 )
  if(missing(hide)) hide = "n"
  if(missing(number_color)) number_color = "b" # default color is black color
  if(missing(threshold)) threshold = c(NA, NA, NA)
  if(missing(plotmode)) plotmode = "alone"
    
  # Set/Short colnames
  if( is.null(colnames(m)) ) colnames( m ) <- as.character( c(1:ncol(m)) )
  else colnames( m ) <- paste( substr(colnames(m), 1,5), ".", sep="" )
  
  # Save graphical parameters
  if( plotmode == "alone" ) oldpar <- par(no.readonly = TRUE)
  else  oldmar <- par("mar") # save margins

  # Create an empty plot without margins
  par( mar = c(0,0,0,0) );  plot.new()           
  plot.window( xlim = c(0, 1), ylim = c(0,1), asp = 1 )

  # Plot Grids 
  n = ncol(m); dx = 1/(n+1); x = seq( dx, 1, by = dx )
  rect(dx,0,x,1-dx,border="black"); rect(dx,0,1,x-dx,border="black")
  
  # PLot Labels
  cs_lab = .cex_size ( cellsize = 1/n, texts = colnames(m), mode = "label" )
  for(i in 1:n) {
    x = dx/2; y = (n-i+0.5)*dx # column labels x,y position
       text( x, y, colnames(m)[i], col = label_color[i], cex = cs_lab )
    x = 1 - (i-0.5)*dx; y = 1 - dx/2-dx/4 # row labels x,y position
       text( x, y, colnames(m)[1+n-i], col = label_color[1+n-i], cex = cs_lab ) 
  }
  
  # PLot Date
  Mtext <- matrix( data <- " ", ncol = n, nrow = n )   # matrix for data as text
  Mcol  <- matrix( data <- 1,   ncol = n, nrow = n )   # matrix for colors of text
  Mx    <- matrix( data <- 0,   ncol = n, nrow = n )   # matrix for x coordinate
  My    <- matrix( data <- 0,   ncol = n, nrow = n )   # matrix for y coordinate
  for(i in 1:n) { for(k in 1:n) { h=0
     if( is.na(m[i,k]) ) h = 1 # data NA
     if( i == k ) if( hide == "d" | hide == "ud" | hide == "ld" ) h = 1 # hide diagonal
     if( i < k )  if( hide == "u" | hide == "ud" ) h = 1 # hide upper part
     if( i > k )  if( hide == "l" | hide == "ld" ) h = 1 # hide lower part
     if( h == 0 ) { # do not hide
       Mx[i,k] = k*dx + dx/2; My[i,k] = (n-i)*dx + dx/2
       if( !is.na(threshold[1]) )
         .mark( Mx[i,k], My[i,k], dx/2, m[i,k], threshold ) # mark it if necessary
       if( is.numeric(m[i,k]) ) #data are numerical
            Mtext[i,k] = format( m[i,k], digits=3, decimal.mark=".")
       else Mtext[i,k] = substr( as.character(m[i,k]), 0,5) #char
       Mcol[i,k] = .setcol( m[i,k], number_color )
     }
  }} # for for end
  cs_mat = .cex_size( cellsize = 1/n, texts = Mtext, mode = "matrix" )
  text( x = Mx, y = My, Mtext, col = Mcol, cex = cs_mat ) # display data

  # Title, subtitle
  cs_tle = cs_lab * 1.1
  #text( x = 0.5, y = 1 - strheight(title), title, cex = cs_tle, font = 2 )
  text( x = 0.5, y = 1 - dx/4, title, cex = cs_tle, font = 2 )
  text( x = 0.5, y = 1 - dx/4 - dx/8, subtitle, cex = cs_tle * 0.9 )
  
  # Restore graphical parameters
  if( plotmode == "alone" ) par(oldpar) # restore graphical parameters
  else par( mar = oldmar ) # restore margins
}

# Examples
# mcplot() #blank plot

# Function ".cex_size" calcultes cex size for given
# "cellsize" cell size and "texts" text matrix or vector
# requires function strwidth() from "grahics" library
.cex_size = function( cellsize, texts, mode )
{
  cs =  cellsize / max( strwidth( texts ) )
  switch( mode,
    "label"  = { cs = cs / 1.05 },      
    "matrix" = { cs = cs / 2 }
  )
  if( cs > 1.5 ) cs = 1.5 # reduce a big font size
  return ( cs )
}

#  m = matrix( c(1:25), nrow = 5, ncol = 5); m[4,2] = -22
#  colnames( m ) <- c("First", "Second", "Third", "Fourth", "Fifth")
#  .mcplot( m = m, title = "Title", subtitle = "( explanations )",
#           hide = "ud", number_color = "rb", threshold = c(7,12,0), 
#           plotmode = "complex")

#  #m = tc$comparison$ccorr_lag0$data # OK
#  m = tc$comparison$ccorr_delay$data # Not OK
#  title = tc$comparison$ccorr_delay$title
#  .mcplot( m = m, title = title, subtitle = "",
#           hide = "ud", number_color = "rb", 
#           plotmode = "complex")
