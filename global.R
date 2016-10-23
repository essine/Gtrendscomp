options(shiny.maxRequestSize = 9*1024^2)                       # raise file size limit to 9 Mb

library(shiny)                                                 # Shiny
library(gtrendsR)                                              # Functions to Perform and Display Google Trends Queries
library(forecast)                                              # Forecasting Functions for Time Series

source("Rscripts/initInputs.R")                                # initial data

source(paste(.RFilesDir,"inlineStyles.R",sep=""))              # inline styles data

.load_rscripts(.RFilesModuleDir)                               # load modules

.load_rscripts(.RFilesAPDir)                                   # load application programs

