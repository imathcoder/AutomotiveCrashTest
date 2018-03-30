library(caret)
setwd("~/R/logistic regression")
rm(list = ls())
library(caret)
crashTest_1 <- read.csv("crashTest_1.csv", row.names = 1)
crashTest_1_TEST <- read.csv("crashTest_1_TEST.csv", row.names = 1)
View
View(crashTest_1)
View(crashTest_1_TEST)
str(crashTest_1)
str(crashTest_1_TEST)
summary(crashTest_1)
summary(crashTest_1_TEST)
logisfit <- glm(formula = crashTest_1$CarType~., family = 'binomial', data = crashTest_1)
logisfit
summary(logisfit)
logisTrain <- predict(logisfit, type = 'response')
plot(logisTrain)
tapply(logisTrain, crashTest_1$CarType, mean)
logisPred <- predict(logisfit, newdata = crashTest_1_TEST, type = 'response')
plot(logisPred)
logisPred
crashTest_1_TEST[logisPred<=0.5,"LogisPred"] <- "Hatchback"
crashTest_1_TEST[logisPred>0.5,"LogisPred"] <- "SUV"
confusionMatrix(table(crashTest_1_TEST[,7],crashTest_1_TEST[,6]), positive = "Hatchback")
