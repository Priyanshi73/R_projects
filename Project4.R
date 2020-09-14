getwd()
setwd("C:\\Users\\Priyanshi\\Downloads\\Data\\project_4")
hr_test <- read.csv("hr_test.csv",stringsAsFactors = F)
hr_train <- read.csv("hr_train.csv",stringsAsFactors = F)
View(hr_train)
dim(hr_test)
dim(hr_train)

setdiff(names(hr_train),names(hr_test))
hr_test$left=NA
hr_test$data="test"
hr_train$data="train"

hr <- rbind(hr_train,hr_test)
dim(hr)

library(dplyr)
glimpse(hr)

##character variables

table(hr$sales)#mean and combine
sum(is.na(hr$sales))
hr %>% 
  group_by(sales) %>% 
  summarise(mean_left=mean(left,na.rm = T))

hr=hr %>% 
  mutate(sales_31=as.numeric(sales %in% c("accounting","hr","marketing")),
         sales_27=as.numeric(sales %in% c("IT","product_mng","sales","support","technical")),
         sales_22=as.numeric(sales %in% c("management","RandD"))) %>% 
  select(-sales)
                             
##
table(hr$salary)#low=0 medium=1 high=2

hr=hr %>% 
  mutate(salary=ifelse(salary=="low",0,
                ifelse(salary=="medium",1,2)))
glimpse(hr)
##
table(hr$satisfaction_level)
table(hr$last_evaluation)
table(hr$number_project)
table(hr$average_montly_hours)
table(hr$time_spend_company)
table(hr$Work_accident)
table(hr$left)
table(hr$promotion_last_5years)
##
#checking for NA
colSums(is.na(hr))

##separate train and test

hr_train=hr %>% filter(data=="train") %>% select(-data)
hr_test=hr %>% filter(data=="test") %>% select(-data,-left)
glimpse(hr_train)
glimpse(hr_test)
dim(hr_test)
dim(hr_train)

set.seed(2)
s=sample(1:nrow(hr_train),.8*nrow(hr_train))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]

dim(hr_train1)
dim(hr_train2)

##logistic

library(car)
fit=lm(left~.-sales_22,data=hr_train1)
sort(vif(fit),decreasing = T)

alias(fit) #remove variables incomplete-- sales_22

log_fit=glm(left~.-sales_22,data=hr_train1,family = "binomial")
log_fit=step(log_fit)
formula(log_fit)
log_fit=glm(left~ satisfaction_level + last_evaluation + number_project + 
              average_montly_hours + time_spend_company + Work_accident + 
              promotion_last_5years + salary + sales_31 + sales_27,data=hr_train,family = "binomial")
summary(log_fit)

#score and validation
library(pROC)
train1.score=predict(log_fit,newdata = hr_train1,type = "response")
auc(roc(hr_train1$left,train1.score))
train2.score=predict(log_fit,newdata = hr_train2,type="response")
auc(roc(hr_train2$left,train2.score))
##auc should be >.84 so will try decision tree

##---------------------Decision Tree----------------------------------------
library(tree)
hr.tree=tree(left~.,data=hr_train1)
length(hr.tree)
score.train1=predict(hr.tree,newdata=hr_train1,type='vector')
score.train2=predict(hr.tree,newdata = hr_train2,type="vector")
head(score.train1)
head(score.train2)
auc(roc(hr_train2$left,score.train2))
auc(roc(hr_train1$left,score.train1))


##---------------------------Random Forest-----------------------------
glimpse(hr_train1)
## always make ur dependent variable as factor

hr_train1$left=as.factor(hr_train1$left)

dim(hr_train1)
library(randomForest)
library(ggplot2)
library(dplyr)
library(cvTools)
library(caTools)

#random forest:hyperparameter- not dependent on data.. guessed by the user
param=list(mtry=c(5,10),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10))

#y-- actual Y
#yhat-- predicted value
mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}
num_trials=50
my_params=subset_paras(param,num_trials)
my_params

myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~., 
             data =hr_train1,
             tuning =params,
             folds = cvFolds(nrow(hr_train1), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  print(score.this)
  print(myauc)
  
  if(score.this>myauc){
    #print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  #print('DONE')
  # uncomment the line above to keep track of progress
}

## Values obtained from an earlier run 

myauc=8394606
best_params=data.frame(mtry=5,
                       ntree=100,
                       maxnodes=100,
                       nodesize=5)

## Model on the entire training data
hr_train$left=as.factor(hr_train$left)
hr.rf.final=randomForest(left~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=hr_train)


## Follow the same process as earlier for prediction on
## test production data

test.score=predict(hr.rf.final,newdata = hr_test,type='prob')[,2]
head(test.score)
write.csv(test.score,'Priyanshi_Agarwal_P4_part2.csv',row.names = F)     

##----------------------------quiz------------------------------------------
library(dplyr)
dim(hr_train)
View(hr_train)
#2
table(hr_train$promotion_last_5years)
#3
table(hr_train$satisfaction_level)
hr_train %>% 
  filter(left==0) %>% 
  summarise(var1=var(satisfaction_level))
#4
table(hr_train$average_montly_hours)
hist(hr_train$average_montly_hours)
#5
hr_train %>% 
  group_by(salary) %>% 
  count(left=="1")
#6
cor(hr_train$last_evaluation,hr_train$average_montly_hours)
#7
hr_train %>% 
  filter(Work_accident==1) %>% 
  count(left)
#8
hr_train %>% 
  filter(left==1) %>% 
  summarise(med=median(time_spend_company))
#9
hr_train %>% 
  group_by(sales) %>% 
  summarise(mm=median(average_montly_hours))
#10
hr_train %>% 
  group_by(number_project) %>% 
  count(left)
