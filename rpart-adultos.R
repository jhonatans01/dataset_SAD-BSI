d <- read.table("Downloads/adult.data2.txt", sep=",",header=T)
#colnames(d) <- c("idade","classe","fnlwgt","educacao","educacao-num","estadoCivil","ocupacao",
#                 "relacionamento","raca","sexo","capital-gain","capital-loss",
#                 "horasPorSemana","paisNativo","income")

#sexo.male <- which(d[,10] == "1")
#d[sexo.male,10] <- "Male"
#sexo.female <- which(d[,10] == "2")
#d[sexo.female,10] <- "Female"

income.under <- which(d[,"income"] == "1")
d[income.under,"income"] <- "<=50K"
income.over <- which(d[,"income"] == "2")
d[income.over,"income"] <- ">50K"

d[,"income"] <- as.factor(d[,"income"])

table(d$income)
prop.table(table(d$income))

library(rpart)
arv <- rpart(income ~ ., data=d)
plot(arv)

library(rattle)
library(RColorBrewer)

fancyRpartPlot(arv)
