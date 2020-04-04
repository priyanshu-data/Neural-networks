
library(neuralnet)#used for neural model
library(plyr)#used for revalue function
library(psych)#used for cor function



#loading the file in Rstudio
comp_startups<-read.csv(choose.files())
str(comp_startups)
View(comp_startups)

comp_startups$State<-as.numeric(revalue(comp_startups$State),c("California"="0","Florida"="1","New York"="2"))#changing alphabet to numeric form

#some visualization technique
hist(comp_startups$R.D.Spend,probability = T,breaks = 20)
lines(density(comp_startups$R.D.Spend))#data density is left skewed
#same for the other variables


#correlation of the dataset,i.e;values between -1 and 1
cor(comp_startups)


# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
comp_startups_norm<-as.data.frame(lapply(comp_startups,FUN=normalize))
summary(comp_startups_norm$Profit) # Normalized form of profit


##data partition
set.seed(123)
ind <- sample(2, nrow(comp_startups_norm), replace = TRUE, prob = c(0.7,0.3))
comp_train <- comp_startups_norm[ind==1,]
comp_test  <- comp_startups_norm[ind==2,]


##building model on training data i.e;comp_train
neural_model<-neuralnet(Profit~R.D.Spend+Administration
                        +Marketing.Spend+State,data = comp_train)
str(neural_model)
plot(neural_model,rep = "best")


# Evaluating model performance
set.seed(1234)
model_evaluate <- compute(neural_model,comp_test[1:4])#evaluation on test data i.e;comp_test###also we don't take 5 variable asit is a target vriable.
evaluate_profit2 <- model_evaluate$net.result


# Predicted profit Vs Actual profit of test data.
cor(evaluate_profit2,comp_test$Profit)#95% accuracy obtained

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
value_max <- max(comp_startups$Profit)
value_min <- min(comp_startups$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(evaluate_profit2,value_min,value_max)
head(ActualProfit_pred)



# Improve the model performance :
set.seed(12345)
company_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = comp_train,
                             hidden = c(4,2))
plot(company_model2 ,rep = "best")#MSE=0.038%



#improving again
set.seed(123456)
company_model3 <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = comp_train,
                            hidden = 2)
plot(company_model3 ,rep = "best")#MSE=0.028%

#improving again
set.seed(1234567)
company_model4 <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = comp_train,
                            hidden = c(2,2))
plot(company_model4 ,rep = "best")#error=0.039%


#from above model improving i can say model3 is best suited model.




model_results3<-compute(company_model3,comp_test[1:4])#as usual 5 variable is excluded because of target varaible
evaluate_profit3<-model_results3$net.result
##predicted profit over actual profit
cor(evaluate_profit3<-model_results3$net.result)
#unnormalizing the data
value_max3 <- max(comp_startups$Profit)
value_min3 <- min(comp_startups$Profit)

unnormalize3 <- function(x, min, max) { 
  return( (max - min)*x + min )}
  

ActualProfit_pred3 <- unnormalize(evaluate_profit3,value_min3,value_max3)
head(ActualProfit_pred3)
  
  
  
  
  
  
  








