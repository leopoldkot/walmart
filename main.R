library('RColorBrewer')
library('ggplot2')
library('scales')
library('reshape2')
library('plyr')
library('forecast')
library('zoo')
require('lubridate')
source('./multiplot.R')
source('./load.R')
source('./regs.R')
source('./utils.R')
source('./plots.R')
source('./tests.R')
source('./predict.R')
library(Matrix)

USE_REGX <- TRUE

init <- function () {
  loadData()
  calcXRegs()
}