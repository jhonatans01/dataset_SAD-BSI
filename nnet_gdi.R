library(nnet)
nn <- nnet(gdi ~ ., data=d, size = 1, rang = 0.5, decay = 0.001, maxit=10)
nnCont <- nnet(continente ~ ., data=d, size = 1, rang = 0.5, decay = 0.001, maxit=10)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)

#ind.teste <- sample(1:nrow(d), 127)
#ind.treino <- (1:nrow(d))[-ind.teste]



library(randomForest)
#arv1 <- rpart(continente ~ ., data=d[ind.treino,])
#pred <- predict(nn, d[ind.teste,], type="raw")
#real <- d[ind.teste, "continente"]
#rf <- randomForest(continente ~ ., data=d[ind.teste,])
#rf.pred <- predict(rf, d[ind.teste,-1], type="response")
#plot(rf)

#randomForest gdi
gdiRModel <- randomForest(gdi ~ ., data=d[-1,])
contRModel <- randomForest(continente ~ ., data=d[-1,])

plot (contRModel)
print(contRModel)


