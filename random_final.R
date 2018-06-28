rando <- read.csv("C:\\Users\\spandan\\Desktop\\Machine Learning Project\\zomato1.csv", header = T, sep = ",", na.strings="Not rated")
rando=na.omit(rando)
library("tidyr")
new <- separate_rows(rando, Cuisines, sep = ",")  
View(new)
new$Restaurant.Name = as.numeric(new$Restaurant.Name)
new$City = as.numeric(new$City)
new$Address = as.numeric(new$Address)
new$Locality = as.numeric(new$Locality)
new$Cuisines = as.factor(new$Cuisines)
new$Cuisines = as.numeric(new$Cuisines)
new$Currency = as.numeric(new$Currency)
new$Has.Online.delivery = as.numeric(new$Has.Online.delivery)
new$Has.Table.booking = as.numeric(new$Has.Table.booking)
new$Rating.color = as.numeric(new$Rating.color)
new$Rating.text = as.numeric(new$Rating.text)
View(new)
library("randomForest")
library("MASS")
set.seed(30)
train=sample(1:11393,replace = T)
obb.err = double(15)
test.err = double(15)
for(mtry in 1:15)
{
  rf=randomForest(Rating.text ~ . , data = new , subset = train, mtry = mtry, ntree=500, importance=T) 
  obb.err[mtry] = rf$mse[500]
  pred<-predict(rf,new[-train,])
  test.err[mtry]= with(new[-train,], mean((Rating.text - pred)^2))
  cat(mtry," ")
}
rf
plot(rf)
test.err
obb.err
matplot(1:mtry , cbind(obb.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
imp = importance(rf)
m_obb = mean(obb.err)
accuracy = (1 - m_obb)*100
accuracy
