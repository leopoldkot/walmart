library('RColorBrewer', quietly=T)
library('ggplot2', quietly=T)
library('scales', quietly=T)
library('reshape2', quietly=T)
library('plyr', quietly=T)
library('forecast', quietly=T)
library('zoo', quietly=T)
library('lubridate', quietly=T)
library('Matrix', quietly=T)

source('./multiplot.R')
source('./load.R')
source('./regs.R')
source('./utils.R')
source('./plots.R')
source('./tests.R')
source('./predict.R')
source('./model.R')
source('./model_arima.R')

USE_REGX <- TRUE
DEFAULT_MODEL <- 'arima'

init <- function () {
  #rm(list=ls(pos=globalenv()))
  loadData()
  calcXRegs()
}