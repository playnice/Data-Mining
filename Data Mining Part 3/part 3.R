library(ggplot2) 
library(caret) 
library(rpart)  
library(rpart.plot) 
library(ROCR) 
library(arules) 
library(e1071) 
library('neuralnet') 
library(ModelMetrics)
setwd("C:/Users/user/Desktop/Part 3")

# 1. datatest.txt
# exploratory data analysis
trainset <-  read.table("datatraining.txt",header=TRUE,sep=",",stringsAsFactors = FALSE)


# pre-processing task 
plot_temp <- ggplot(trainset, aes(x='', y = Temperature)) + geom_boxplot(color="red") + xlab("Temperature") + ylab("Degree Celcius")
plot_humidity <- ggplot(trainset, aes(x='', y = Humidity)) + geom_boxplot(color="orange")+ xlab("Humidity")+ ylab("%") 
plot_light <- ggplot(trainset, aes(x='', y = Light)) + geom_boxplot(color="black")+ xlab("Light") + ylab("Lux") 
plot_co2 <- ggplot(trainset, aes(x='',y = CO2)) + geom_boxplot(color="green")+ xlab("CO2")+ylab("ppm") 
plot_ratio <- ggplot(trainset, aes(x='', y = HumidityRatio)) + geom_boxplot(color="blue")+ xlab("HumidityRatio")+ ylab("kgwater-vapor/kg-air ") 
plot_occupancy <- ggplot(trainset, aes(x = Occupancy, fill = as.factor(Occupancy))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), colour = 'black') +
  scale_y_continuous(("Count"), labels=percent) + scale_fill_discrete("Occupancy")

print(plot_temp)
print(plot_humidity)
print(plot_light)
print(plot_co2)
print(plot_ratio)
print(plot_occupancy)


##################
# Decision Tree  #
##################
trainset$Occupancy = as.factor(trainset$Occupancy)
trainsetTree = trainset
trainsetTree <- subset(trainsetTree, select = -c(date))

tree <- rpart(Occupancy ~ Temperature+Humidity+Light+CO2+HumidityRatio , data=trainsetTree,control = rpart.control(cp = 0.0001))
prp(tree, faclen = 0, cex = 0.8, extra = 1)
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp = bestcp)
summary(tree.pruned)

conf.matrix <- table(trainsetTree$Occupancy, predict(tree.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
plot(tree.pruned)
text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)

##################
#      ANN       #
##################

maximums <- apply(trainset[,2:6],2, max)
minimums <- apply(trainset[,2:6],2, min)
trainANN <- as.data.frame(scale(trainset[,2:6],center = minimums, scale = maximums - minimums))
trainANN <- cbind(Occupancy=as.numeric(trainset$Occupancy)-1, trainANN)
feats <- names(trainANN)
f <- paste(feats[2:6],collapse=' + ')
f <- paste(feats[1], ' ~ ',f)
f <- as.formula(f)
nn <- neuralnet(f,trainANN,hidden=c(3,2,1), linear.output = FALSE)

##################
#   Naive Bayes  #
##################

trainNB = trainset
for(i in 2:6){
  trainNB[[i]] <- discretize(trainNB[[i]], method = "interval",4,labels=c("Low","Medium","High","Very High"))
}
predNB <- naiveBayes(Occupancy~ . -date, trainNB)

######################
#   Tests set(TREE)  #
######################

testsetTree <- read.table("datatest2.txt", header =TRUE,sep=",", stringsAsFactors = FALSE)
testsetTree$Occupancy <- as.factor(testsetTree$Occupancy)
testsetTree<- subset(testsetTree , select = -c(date))
tree.pred <- predict(tree.pruned, newdata = testsetTree, type = 'class')
confusion.pred <- table(tree.pred, testsetTree$Occupancy)
rownames(confusion.pred) <- paste("Actual", rownames(confusion.pred), sep = ":")
colnames(confusion.pred) <- paste("Pred", colnames(confusion.pred), sep = ":")
confusion.pred
ROCTree <- ROCR::prediction(as.numeric(tree.pred), as.numeric(testsetTree$Occupancy))
plot(performance(ROCTree, measure="tpr", x.measure="fpr"), colorize=TRUE)

#######################
#     Tests set(NB)   #
#######################

testNB <- read.table("datatest2.txt",header=TRUE,sep=",",stringsAsFactors = FALSE)
testNB$Occupancy = as.factor(testNB$Occupancy)
for(i in 2:6){
  testNB[[i]] <- discretize(testNB[[i]], method = "interval",4,labels=c("Low","Medium","High","Very High"))
}
predB <- predict(predNB, newdata = testNB[,2:6])
confusionMatrix(predB, testNB$Occupancy)
ROCNB <- ROCR::prediction(as.numeric(predB), as.numeric(testNB$Occupancy))
plot(performance(ROCNB, measure="tpr", x.measure="fpr"), colorize=TRUE)

#######################
#   Tests set(ANN)   #
#######################


testANN <- read.table("datatest2.txt",header=TRUE,sep=",",stringsAsFactors = FALSE)
testANN$Occupancy = as.numeric(testANN$Occupancy)
maximums <- apply(testANN[,2:6],2, max)
minimums <- apply(testANN[,2:6],2, min)
scaled.dataANN <- as.data.frame(scale(testANN[,2:6],center = minimums, scale = maximums - minimums))
testANN <- cbind(Occupancy=testANN$Occupancy, scaled.dataANN )
predN <- compute(nn,testANN[,2:6])
predN <- sapply(predN$net.result,round,digits=0)
confusionMatrix(predN, testANN$Occupancy)
ROCNN <- ROCR::prediction(predN, as.numeric(testANN$Occupancy))
plot(performance(ROCNN, measure="tpr", x.measure="fpr"), colorize=TRUE)


##############
# ROC CURVES #
##############

DT <- performance( ROCTree, "tpr", "fpr" )
BN <- performance(ROCNB, "tpr", "fpr")
ANN <- performance(ROCNN, "tpr", "fpr")
plot( DT, col = 'red')
plot( BN, add = TRUE, col = 'blue')
plot( ANN, add = TRUE, col = 'green')
legend(0.6,0.6, legend=c("Decision Tree", "Naive Bayes", "ANN"),
       col=c("red", "blue", "green"), lty=1, cex=0.8)

save(DT,BN,ANN,file = 'classifiers.RO')

