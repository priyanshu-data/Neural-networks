
library(neuralnet)##for regression
library(nnet)#classification
library(NeuralNetTools)#
library(plyr)#correlation


#Read the data
startups<-read.csv(choose.files())
View(startups)
class(startups)

startups$State<-as.numeric(revalue(startups$State,
                                   c("New York"="0","California"="1","Florida"="2")))
startups<-as.data.frame(startups)
attach(startups)

##exploratory data analysis
plot(R.D.Spend,Profit)
plot(Administration,Profit)
plot(Marketing.Spend,Profit)
plot(State,Profit)
windows()
pairs(startups)#finding correlation between o/p i.e;profit and inputs(3 variables)
#corelation coefficient
cor(startups)
summary(startups)
#normalizing the data set
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
startups_norm<-as.data.frame(lapply(startups,FUN = normalize))
summary(startups_norm$Profit)#normalized form of profit
summary(startups$Profit)#original profit value


#Data partition
set.seed(123)
ind<-sample(2,nrow(startups_norm),replace = TRUE,prob = c(0.7,0.3))
startups_train<-startups_norm[ind==1,]
startups_test<-startups_norm[ind==2,]
#creating neural network on training data
startups_model<-neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = startups_train)
str(startups_model)
plot(startups_model)
summary(startups_model)

#some fun with parameters
par(mar = numeric(4), family = 'serif')
plotnet(startups_model, alpha = 0.6)

#evaluating model performance
set.seed(1234)
model_results<-compute(startups_model,startups_test[1:4])
predicted_profit<-model_results$net.result

#predicted profit vs actual profit of test data
cor(predicted_profit,startups_test$Profit)##95% accuracy

#since prediction is in normalize form ,we need to denormalize it
#to get actual prediction on profit
str_max<-max(startups$Profit)
str_min<-min(startups$Profit)

unnormalize<-function(x,min,max){return((max-min)*x+min)}
actual_profit_pred<-unnormalize(predicted_profit,str_min,str_max)
head(actual_profit_pred)

#improve the model performance
set.seed(12345)
startups_model2<-neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = startups_train,hidden=2)
plot(startups_model2,rep = "best")
summary(startups_model2)
model_results2<-compute(startups_model2,startups_test[1:4])
predicted_profit2<-model_results2$net.result
cor(predicted_profit2,startups_test$Profit)##96%accuracy
plot(predicted_profit2,startups_test$Profit)
par(mar =numeric(4),family='serif')
plotnet(startups_model2,alpha = 0.6)

  
  
  








