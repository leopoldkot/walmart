
testLM <- function (store, dept) {
  df <- subset(train, Store==store & Dept == dept)[,c('Date', 'Weekly_Sales', 'WeekOfTheYear')]
  df <- cbind(df, train.xreg)
  
  fit <- rlm(Weekly_Sales ~ ., data=df)
  #print(summary(fit))
  #ws_p <- predict(fit)
  #df <- cbind(df, ws_p)
  
  #print(ggplot(data=df) + geom_line(aes(x=Date, y=Weekly_Sales, colour='green')) + geom_line(aes(x=Date, y=ws_p, colour='red')))
  #df <- data.frame()join(df, train.xreg)
  #df$ws_lm_diff <- df$Weekly_Sales - df$ws_p
  #print(plot(stl(ts(df$ws_lm_diff, frequency=52), s.window='periodic')))
  return(df)
}