wine <- read.csv("winequality-white.csv", sep=';',check.names = FALSE)
colnames<-dimnames(wine)[[2]]
par(mfrow=c(2, 2))
for (i in 1:4) {hist(wine[,i], main=colnames[i], col="gray", border="white",freq = NULL,xlab = NULL,ylab= NULL)}
for (i in 5:8) {hist(wine[,i], main=colnames[i], col="gray", border="white",freq = NULL,xlab = NULL,ylab= NULL)}
for (i in 9:12) {hist(wine[,i], main=colnames[i],  col="gray", border="white",freq = NULL,xlab = NULL,ylab= NULL)}
par(mfrow=c(1, 5))
for (i in 1:5) {boxplot(wine[,i], main=colnames[i], xlab = NULL,ylab= NULL)}
par(mfrow=c(1, 5))
for (i in 6:11) {boxplot(wine[,i], main=colnames[i], xlab = NULL,ylab= NULL)}
remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75))
  H <- 1.5 * IQR(x)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
outliersRemoved <- remove_outliers(wine$`residual sugar`)
boxplot(outliersRemoved)
hist(outliersRemoved)
wineNoOut <- as.data.frame(sapply(wine,remove_outliers))
winenNoOut <- na.omit(wineNoOut)
winenNoOutCategory <- winenNoOut
winenNoOutCategory["category"]<-NA
winenNoOutCategory$category<- ifelse(winenNoOutCategory$quality== 3, "Low",
ifelse(winenNoOutCategory$quality== 4, "Low",
ifelse(winenNoOutCategory$quality== 5, "Low",
ifelse(winenNoOutCategory$quality==6, "Medium",
ifelse(winenNoOutCategory$quality==7, "High",
ifelse(winenNoOutCategory$quality== 8, "High",
ifelse(winenNoOutCategory$quality== 9, "High",NA
)))))))
corrplot(cor(winenNoOutCategory[1:12]), method="number",tl.col="red", col = "black")


