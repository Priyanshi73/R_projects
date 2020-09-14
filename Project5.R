getwd()
setwd("C:\\Users\\Priyanshi\\Downloads\\Data\\project_5")
bank_full_test=read.csv("bank-full_test.csv",stringsAsFactors = F)
bank_full_train=read.csv("bank-full_train.csv",stringsAsFactors = F)
View(bank_full_train)
dim(bank_full_test)
dim(bank_full_train)

setdiff(names(bank_full_train),names(bank_full_test))
bank_full_test$y=NA
bank_full_test$data="test"
bank_full_train$data="train"

bank <- rbind(bank_full_train,bank_full_test)
dim(bank)

library(dplyr)
glimpse(bank)

##character variable

table(bank$job)#dummy encoding
table(bank$marital) ## 0-divorced 1-single 2-married
table(bank$education) #0-pri 1-sec 2-tert 3-unknown
table(bank$default) #0 1
table(bank$housing) #0 1
table(bank$loan) # 0 1
table(bank$contact) ## 0-cellu 1-tele 3-unknown
table(bank$month)#dummy
table(bank$poutcome) ##0-fail 1-success 2-other 3-unknown
table(bank$y) #0 1

bank=bank %>% 
  mutate(default=as.numeric(default=="yes"),
         housing=as.numeric(housing=="yes"),
         loan=as.numeric(loan=="yes"),
         y=as.numeric(y=="yes"))
glimpse(bank)   

##education #0-pri 1-sec 2-tert 3-unknown
##contact # 0-cellu 1-tele 3-unknown
#failure  ##0-fail 1-success 2-other 3-unknown
#marital ## 0-divorced 1-single 2-married
bank=bank %>% 
  mutate(education=ifelse(education=="primary",0,
                          ifelse(education=="secondary",1,
                             ifelse(education=="tertiary",2,3))),
         contact=ifelse(contact=="cellular",0,
                        ifelse(contact=="telephone",1,2)),
         poutcome=ifelse(poutcome=="failure",0,
                         ifelse(poutcome=="success",1,
                                ifelse(poutcome=="other",2,3))),
         marital=ifelse(marital=="divorced",0,
                        ifelse(marital=="single",1,2)))
glimpse(bank)         
  #job-- group by mean      
bank %>% 
  group_by(job) %>% 
  summarise(mean_job=mean(y,na.rm=T))

bank=bank %>% 
  mutate(job_11=as.numeric(job %in% c("admin","self-employed","technician","unknown")),
         job_13=as.numeric(job %in% c("management","unemployed")),
         job_07=as.numeric(job %in% c(" blue-collar","housemaid","entrepreneur","services")),
         job_20=as.numeric(job %in% c("retired","student"))) %>% 
 select(-job)

 ##month -- group by mean
bank %>% 
  group_by(month) %>% 
  summarise(mean_job=mean(y,na.rm=T))
         
bank=bank %>% 
  mutate(month_4=as.numeric(month %in% c("dec","oct","sep","marc")),
         month_06=as.numeric(month %in% c("may","jul")),
         month_09=as.numeric(month %in% c("jun","nov","jan","aug")),
         month_16=as.numeric(month %in% c("feb","apr"))) %>% 
  select(-month)
glimpse(bank)

##

table(bank$age)
sort(table(bank$balance),decreasing = T)[1:5]
table(bank$day)
table(bank$duration)
table(bank$campaign)
table(bank$pdays)
table(bank$previous)
table(bank$ID) 
##-----------

#checking for NA

colSums(is.na(bank))
dim(bank)

bank_full_test=bank %>% filter(data=="test") %>% select(-data,-y)
bank_full_train=bank %>% filter(data=="train") %>% select(-data)

glimpse(bank_full_train)
dim(bank_full_train)
##

s=sample(1:nrow(bank_full_train),.8*nrow(bank_full_train))
bank_train1=bank_full_train[s,]
bank_train2=bank_full_train[-s,]
##

library(car)

for_vif=lm(y~.-ID,data=bank_train1)
sort(vif(for_vif),decreasing = T)
#removed vif>4
for_vif=lm(y~.-ID-month_06,data=bank_train1)
sort(vif(for_vif),decreasing = T)

#glm

log_fit=glm(y~.-ID-month_06,data=bank_train1,family = "binomial")
log_fit=step(log_fit)
formula(log_fit)

log_fit=glm(y ~ marital + education + balance + housing + 
              loan + contact + duration + campaign + previous + 
              poutcome + job_07 + job_20 + month_4+ 
              month_16,data=bank_full_train,family = "binomial")
summary(log_fit)

##scoring auc

library(pROC)
train1.score=predict(log_fit,newdata = bank_train1,type="response")
auc(roc(bank_train1$y,train1.score))
train2.score=predict(log_fit,newdata = bank_train2,type="response")
auc(roc(bank_train2$y,train2.score))

train.score=predict(log_fit,newdata = bank_full_train,type="response")


real=bank_full_train$y #positivies
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  print(paste0("cutoff is:", cutoff))
  predicted=as.numeric(train.score>cutoff) #call it 1
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P #sensitivity
  Sp=TN/N #specificity
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]
View(cutoff_data)

#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()

## to see whr is ur maxx cutoff and KS

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
my_cutoff
View(cutoff_data) #look KS at cotoff=.121
#ks=.624

#now we have KS we will predict with train and test
train.score
train.predicted=as.numeric(train.score>my_cutoff)
test.score=predict(log_fit,newdata = bank_full_test,type="response")
test.predicted=as.numeric(test.score>my_cutoff)
head(test.predicted)
dim(bank_full_test)
length(test.predicted)

write.csv(test.predicted,"Priyanshi_Agarwal_P5_part2.csv",row.names = F)
##
##---------------------------------Quiz-------------------------------------

View(bank_full_train)

#1 40.91
mean(bank_full_train$age,na.rm = T)
#2 3329
table(bank_full_train$balance)
#upper
quantile(bank_full_train$balance,.75)+1.5*IQR(bank_full_train$balance)
sum(bank_full_train$balance>3427) #3315
#lower
quantile(bank_full_train$balance,.25)-1.5*IQR(bank_full_train$balance)
sum(bank_full_train$balance<(-1941)) #14

#3
var(bank_full_train$balance)
#4-- vif
#5-- lower AIC
#6-- no
#7-- yes
#8-- balanced
#9-- Q3+1.5*IQR
#10-- adjusted R2
##------------------------------------------------------------------