require(quantmod)
require(caret)

ts <- getStoreDeptTrainTS(1, 1)

T = as.numeric(time(ts))*52
y = as.numeric(ts)

dat <- data.frame(y, x1=Lag(y, 1), x2=Lag(y, 2), x3=Lag(y, 52), x4=(T %% 52))
names(dat) <- c('y','x1','x2', 'x3', 'x4')

model <- train(y ~ x1 + x2,
               data=dat, 
               method='rbf',
               trControl = trainControl(method = "cv")
               )

ps <- predict(model, newdata=dat)

#dat$yhat <- 



#Examine results

plot(T,y,type="l",col = 2)
lines(T[-c(1:52)], ps, col=3)
legend(5, 70, c("y", "pred"), cex=1.5, fill=2:3)