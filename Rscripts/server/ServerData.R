# Help
observeEvent( input$DataHelpBtn, {
  callModule( .HelpPage,id="DataHelpPage", 
              helphtmlpage = .HelpHtmlFileName["Data"], 
              helpcss=.HelpCSSFileName, 
              idcopy="DataHelpPage")
})

# on data selection change
observeEvent( input$DataSelectRBtn, {
 if( input$MainTabSet == "DataTabPanel") display_dataframe()
})

# display data
display_dataframe <- function() {
  if( is.null( names(.GlobalEnv$td)  ) ) { # no google data yet
      .GlobalEnv$.ErrorStatus$Data <- TRUE
      callModule( .ErrorMessage, id="DataErrorMessage", 
                  title = "Data:", 
                  errormessage = "data are not available",
                  idcopy="DataErrorMessage")
  }    
  else {
    selecteddata <- datasetInput(input$DataSelectRBtn)
    .GlobalEnv$.ErrorStatus$Data <- FALSE
    output$DataTitle <- renderText(paste("Data: ",selecteddata$name))
    output$DataAsTable <- renderDataTable(selecteddata$data)
  }  
}

# Rearrange ARIMA coefficients to a matrix
arima_coefficents_matrix <- function()
{
  ntrends = length(tm$coefficients)
  # max number of AR and MA 
  pdq_m = as.matrix(as.data.frame(tm$pdq))
  AR_n = max(pdq_m[1,]); MA_n = max(pdq_m[3,])
  
  # 'p d q' data 
  pdq <- c()
  for(i in 1:ntrends) pdq[i] <- paste(tm$pdq[[i]][1],tm$pdq[[i]][2],tm$pdq[[i]][3]) 
  arma_data = rbind(pdq)

  matrix_colnames = c("p d q")
  # AR coefficients
  if(AR_n!=0){ # If any AR coefficient exist
    for(k in 1:AR_n) { ar = c(); var = c()
      for(i in 1:ntrends) { 
        if( k <= nrow( tm$coefficients[[i]] ) ) {
          ar[i] = tm$coefficients[[i]][k,1]
          var[i] = tm$coefficients[[i]][k,2]  
          if(ar[i] == 0 ) { ar[i] = NA; var[i] = NA  }
        }
        else { ar[i] = NA; var[i] = NA  }
      }
      matrix_colnames = c( matrix_colnames, c(paste("AR",k,sep=""),"var.coef") )
      arma_data = rbind(arma_data,ar,var)
    }
  }
  # MA coefficents
  if(MA_n!=0){ # If any MA coefficient exist
    for(k in 1:MA_n) { ma = c(); var = c()
      for(i in 1:ntrends) { 
        if( k <= nrow( tm$coefficients[[i]] ) ) {
          ma[i] = tm$coefficients[[i]][k,3]
          var[i] = tm$coefficients[[i]][k,4]   
          if(ma[i] == 0 ) { ma[i] = NA; var[i] = NA  }
        }
        else { ma[i] = NA; var[i] = NA  }
      } 
      matrix_colnames = c( matrix_colnames, c(paste("MA",k,sep=""),"var.coef") )
      arma_data = rbind(arma_data,ma,var)
    }
  }
  
  drift_var = as.matrix(as.data.frame(tm$constant_var))
  arma_data = rbind(arma_data, drift_var[1,], drift_var[2,],
                    tm$sigma2, tm$loglik, tm$bic, tm$aic, tm$aicc )
  
  matrix_colnames = c( matrix_colnames, c("constant","var.coef","sigma2","log likehood","BIC","AIC","AICC") )
  arma_data = cbind( "traces" = matrix_colnames, arma_data )
  rownames(arma_data) <- c()
  return(arma_data)
}

# Select data to view
datasetInput <- function (selectedchoice) {
  selecteddata <- list(name="no data",data=data.frame())
    switch(selectedchoice,
         "original" = { selecteddata$name = "td$data"; selecteddata$data = td$data },
         "seasonal" = { selecteddata$name = "tdd$seasonal$data"; selecteddata$data = tdd$seasonal$data },
         "seasonal_trend" = { selecteddata$name = "tdd$trend$data"; selecteddata$data = tdd$trend$data },
         "seasonal_remainder" = { selecteddata$name = "tdd$remainder$data"; selecteddata$data = tdd$remainder$data },
         "smoothed" = { selecteddata$name = "tdd$smoothed$data"; selecteddata$data = tdd$smoothed$data },
         "smoothed_remainder" = { selecteddata$name = "tdd$remainder$data"; selecteddata$data = tdd$remainder$data },
         "differenced" = { selecteddata$name = "tdd$differenced$data"; selecteddata$data = tdd$differenced$data },
         
         "model_residuals" = { selecteddata$name = "tm$residuals$data"; selecteddata$data = tm$residuals$data },
         "model_parameters" =  { selecteddata$name = "ARIMA model: tm$pdq, tm$coefficients, ..."
                                 selecteddata$data = arima_coefficents_matrix() },
         
         "model_lbtest" = { selecteddata$name = "Ljung-Box Test: tm$lb_test$p.value, tm$lb_test$Xsquared, tm$lb_test$df"; 
                            selecteddata$data = rbind(tm$lb_test$p.value, tm$lb_test$Xsquared, tm$lb_test$df)
                            selecteddata$data = cbind(c("p value","X-squared","df"),selecteddata$data)  },
         
         "acorr_sgn" = {selecteddata$name = "tc$comparison$acorr_sgn0$data"; selecteddata$data = tc$comparison$acorr_sgn$data},
         "ccorr_lag0" = {selecteddata$name = "tc$comparison$ccorr_lag0$data"; selecteddata$data = tc$comparison$ccorr_lag0$data},
         "ccorr_max"  = {selecteddata$name = "tc$comparison$ccorr_max$data"; selecteddata$data = tc$comparison$ccorr_max$data},
         "ccorr_max_lag" = {selecteddata$name = "tc$comparison$ccorr_delay$data"; selecteddata$data = tc$comparison$ccorr_delay$data},
         "synchrony" = {selecteddata$name = "tc$comparison$synchrony$data"; selecteddata$data = tc$comparison$synchrony$data}
    )
  return (selecteddata)
}

# Download data
output$DataDownloadBtn <- downloadHandler(
  filename = function() { 
    paste(input$DataSelectRBtn,'.csv', sep='') 
  },
  content = function(file) {
    selecteddata <- datasetInput(input$DataSelectRBtn)
    write.csv(selecteddata$data, file, row.names = FALSE)
  }
)

output$DataDownloadAllBtn <- downloadHandler(
  filename = 'csv.zip',
  content = function(fname) {
    tmpdir <- tempdir(); wd <- getwd(); setwd(tempdir())
    k=length(.GlobalEnv$.viewdatachoices$actual)
    for ( i in 1:k ) {
      selecteddata = datasetInput(.GlobalEnv$.viewdatachoices$actual[i])
      fn = paste(.GlobalEnv$.viewdatachoices$actual[i],".csv",sep="")
      write.csv(selecteddata$data, file = fn, row.names = FALSE)
    }
    fs = paste(.viewdatachoices$actual,".csv",sep="")
    zip(zipfile=fname, files= fs)
    setwd(wd)
  },
  contentType = "application/zip"
)
