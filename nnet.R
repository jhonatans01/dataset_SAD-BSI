d <- read.table("Downloads/adult.data2.txt", sep=",",header=T)

library(nnet)
nn <- nnet(income ~ ., data=d, size = 1, rang = 1, decay = 0.001, maxit=32561)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)

ind.teste <- sample(1:nrow(d), 283)
ind.treino <- (1:nrow(d))[-ind.teste]



library(randomForest)
arv1 <- rpart(income ~ ., data=d[ind.treino,])
pred <- predict(arv1, d[ind.teste,], type="class")
#model <- randomForest(income ~ ., data=d[income.over,])
real <- d[ind.teste, "income"]
rf <- randomForest(income ~ ., data=d[ind.teste,])
rf.pred <- predict(rf, d[ind.teste,-1], type="class")

plot(rf)
