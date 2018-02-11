d <- read.table("Downloads/gender_development(local)(2).csv", sep=",", header = T)
colnames(d) <- c("hdi_rank","pais","continente","gdi","idh_M","idh_H","expecVida_M","expecVida_H",
                 "expecEduc_M","expecEduc_H","mediaAnosEduc_M","mediaAnosEduc_H",
                 "rendNacionBruto_M","rendNacionBruto_H")

#classificar por continentes
levels(d$continente) <- 1:7

#1 - Africa
#2 - Asia
#3- Central America
#4- Europe
#5- North America
#6- Oceania
#7 - South America


#tratar NAs
d <- apply(d, 2, as.numeric)

d <- data.frame(d)

ind.na <- which(is.na(d[,4]))
d1 <- d[-ind.na,]

d2 <- 0
for (i in 5:14) {
  ind.na <- which(is.na(d[,i]))
  if(is.vector(ind.na)) {
    print(ind.na)
    d2 <- d1[-ind.na,]
  }
}

d <- d2

#remover objetos
rm(d2,d1,i,ind.na)


#selecionar colunas
d <- subset(d, select = c("hdi_rank","continente","gdi"))
aux <- which(d[,"gdi"] >= 1)
d <- d[-aux,]

#reclassificar continentes
cont.africa <- which(d[,2] == 1)
cont.asia <- which(d[,2] == 2)
cont.centralA <- which(d[,2] == 3)
cont.europe <- which(d[,2] == 4)
cont.northA <- which(d[,2] == 5)
cont.oceania <- which(d[,2] == 6)
cont.southA <- which(d[,2] == 7)
d[cont.africa,2] <- "Africa"
d[cont.asia,2] <- "Asia"
d[cont.centralA,2] <- "America Central"
d[cont.europe,2] <- "Europa"
d[cont.northA,2] <- "America do Norte"
d[cont.oceania,2] <- "Oceania"
d[cont.southA,2] <- "America do Sul"
#transformar em factor
d[,2] <- as.factor(d[,2])


#remover objetos
rm(cont.africa,cont.asia,cont.oceania,cont.europe,cont.southA,cont.northA,cont.centralA, aux)

library(rpart)
arvCont <- rpart(continente ~ ., data=d)
arvGdi <- rpart(gdi_class ~ ., data=d)


plot(arvCont)
text(arvCont)

#plot(arvGdi)
#text(arvGdi)

library(rattle)
library(RColorBrewer)

fancyRpartPlot(arvCont)


#tabela de probalidades
prop.table(table(d$continente))
prop.table(table(d$gdi_class))