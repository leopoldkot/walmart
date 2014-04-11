library('RColorBrewer')
library('ggplot2')
library('scales')
library('reshape2')
library('plyr')
library('forecast')
library('zoo')
require('lubridate')
source('./multiplot.R')

readSubmit <- function(f) {
  s <- read.csv(f)
  s$Id <- as.character(s$Id)
  tmp <- strsplit(s$Id, '_')
  tmp <- data.frame(matrix(unlist(tmp), ncol=3, byrow=T))
  s$Store <-as.numeric(as.character(tmp[[1]]))
  s$Dept <-as.numeric(as.character(tmp[[2]]))
  s$Date <-as.Date(tmp[[3]])
  return (s)
}

plotPredicts <- function (p1, store) {
  fields <- c('Store', 'Dept', 'Date', 'Weekly_Sales', 'IsHoliday')
  
  cmp <- expression(Store == store & Dept >= 0 & Dept < 10)
  
  #t <- rbind(subset(p1, eval(cmp))[,fields], subset(p2, eval(cmp))[,fields])
  t <- subset(p1, eval(cmp))[,fields]
  print(head(t))
  p <- ( ggplot() 
         + geom_bar(data=t, aes(x=Date,y=(Weekly_Sales), fill=(Dept),order=Dept), stat="identity", position = "stack")
         + facet_grid(Dept ~ ., scale="free_y")
         + geomHolidays()
         + geomMilestones()
  )
  multiplot(p)
}

loadSubmits <- function() {
  p1 <<- readSubmit('./data/submit_20140401_155855_2789.csv')
  p2 <<- readSubmit('./data/submit_20140320_004354_2805.csv')
}

checkSubmit <- function(path) {
  p1 <- readSubmit(path)
  if (nrow(p1[is.na(p1$Weekly_Sales) | is.nan(p1$Weekly_Sales) | is.infinite(p1$Weekly_Sales) ,]) > 0) {
    print('BAD');
  } else {
    print('OK');
  }
}

ssModel <- function (x, a, b, c, d) {
  c*exp(-(a*(x - b))^2) + d;
}

easterWeeks <- EASTER + 52 * seq(0, length(EASTER) - 1)

filterExpGen <- function (a) {
  a <- as.numeric(a)
  return(function (x) {
    #print(abs(x-a) > 1)
    t1 <- ifelse(abs(x-a) > 2, 1, 0)
    #print(t1)
    
    return(t1)
#    1 - exp(-(0.7 * (x - a))^2);  
  })
}

easterFilter <- function (x) {
  x <- as.numeric(x)
  prod(sapply(lapply(easterWeeks, filterExpGen), function(f) {f(x)}))
}

easterFilterVec <- (function() {
  return(Vectorize(easterFilter, 'x'))
})()



#prod(sapply(lapply(easterWeeks, filterExpGen), function(f) {f(163)}))

