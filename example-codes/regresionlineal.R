#In this code we are doing an example of linear regression usin R code.



#Load data
library(MASS)
data("Boston")
View(Boston)

?Boston


set.seed(2)
install.packages("caTools")
library(caTools)

split<-sample.split(Boston$medv,SplitRatio=0.7)
#here we divide the data with the ratio 0.7

split

training_data<-subset(Boston,split=="TRUE")
testing_data<-subset(Boston,split=="FALSE")

plot(Boston$crim,Boston$medv,cex=0.5,xlab = "Crime rate", ylab = "Price")
cr<-cor(Boston)
#here we can se how the crime rate drecrease as well as the price increases


#creating a scatterplot matrix
attach(Boston)
library(lattice)
splom(~Boston[c(1:6,14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)
splom(~Boston[c(7:14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)

#rm & medv
plot(rm,medv)
abline(lm(medv~rm),col="red")

library(corrplot)

corrplot(cr,type="lower")
corrplot(cr,method="number")

install.packages("caret")
library(caret)

Boston_a=subset(Boston,select=-c(medv))
numericData<-Boston_a[sapply(Boston_a,is_numeric)]
descrCor<-cor(numericData)

install.packages("car")
library(car)

model<-lm(medv~.,data=training_data)
vif(model)

summary(model)

predic<-predict(model,testing_data)
predic

plot(testing_data$medv,type = "l",lty=1.8,col="green")
lines(predic,type = "l",col="blue")

