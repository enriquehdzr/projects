library(rpart)
library(rpart.plot)
data("msleep")
str(msleep)
df<-msleep[,c(3,4,6,10,11)]
#here we clean data in orden to get just a few interesting considerations
str(df)
head(df)

m1<-rpart(sleep_total~.,data=df,method="anova")
m1

rpart.plot(m1,type=3,digits=3,fallen.leaves=TRUE)
#we started the split wiht the sleep time. 

p1<-predict(m1,df)
p1  

 