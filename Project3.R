getwd()
setwd("C:\\Users\\Priyanshi\\Downloads\\Data\\project_3")
pr_test=read.csv("product_test.csv",stringsAsFactors = F)
pr_train=read.csv("product_train.csv",stringsAsFactors = F)
View(pr_train)
dim(pr_train)
dim(pr_test)
library(dplyr)

setdiff(names(pr_train),names(pr_test))
pr_test$went_on_backorder=NA
pr_test$data="test"
pr_train$data="train"

pr <- rbind(pr_train,pr_test)
dim(pr)
library(dplyr)
glimpse(pr)

#character variables
table(pr$potential_issue)
table(pr$deck_risk)
table(pr$oe_constraint)
table(pr$ppap_risk)
table(pr$stop_auto_buy)
table(pr$rev_stop)
table(pr$went_on_backorder)
pr=pr %>% 
  mutate(potential_issue=as.numeric(potential_issue=="Yes"),
         deck_risk=as.numeric(deck_risk=="Yes"),
         oe_constraint=as.numeric(oe_constraint=="Yes"),
         ppap_risk=as.numeric(ppap_risk=="Yes"),
         stop_auto_buy=as.numeric(stop_auto_buy=="Yes"),
         rev_stop=as.numeric(rev_stop=="Yes"),
         went_on_backorder=as.numeric(went_on_backorder=="Yes"))
glimpse(pr)

##
sort(table(pr$national_inv),decreasing = T)
sort(table(pr$lead_time),decreasing = T)
sort(table(pr$in_transit_qty),decreasing = T)
sort(table(pr$forecast_3_month),decreasing = T)
sort(table(pr$sales_1_month),decreasing = T)
sort(table(pr$min_bank),decreasing = T)
sort(table(pr$pieces_past_due),decreasing = T)
sort(table(pr$perf_6_month_avg),decreasing = T)
sort(table(pr$local_bo_qty),decreasing = T)

##checking for NA
colSums(is.na(pr))
names(pr)[sapply(pr,function(x)is.character(x))]

##
pr_train=pr %>% filter(data=="train") %>% select(-data)
pr_test=pr %>% filter(data=="test") %>% select(-data,-went_on_backorder)
dim(pr_test)
dim(pr_train)
glimpse(pr_train)
glimpse(pr_test)
View(pr_train)
##

set.seed(2)
s=sample(1:nrow(pr_train),.8*nrow(pr_train))
pr_train1=pr_train[s,]
pr_train2=pr_train[-s,]

dim(pr_train)
dim(pr_train1)
dim(pr_train2)

##logistic

library(car)
fit=lm(went_on_backorder~.-sku,data=pr_train1)
sort(vif(fit),decreasing = T)

fit=lm(went_on_backorder~.-sku-forecast_6_month-sales_6_month-sales_9_month-forecast_9_month-sales_1_month-perf_12_month_avg-min_bank,data=pr_train1)
sort(vif(fit),decreasing = T)

#glm
log_fit=glm(went_on_backorder~.-sku-forecast_6_month-sales_6_month-sales_9_month-forecast_9_month-sales_1_month-perf_12_month_avg-min_bank,data=pr_train1,family = "binomial")
log_fit=step(log_fit)
formula(log_fit)

log_fit=glm(went_on_backorder ~ lead_time +
               pieces_past_due + perf_6_month_avg + local_bo_qty + 
              deck_risk + rev_stop,data=pr_train,family="binomial")

summary(log_fit)

## score and validation

library(pROC)
train2.score=predict(log_fit,newdata = pr_train2,type = "response")
train1.score=predict(log_fit,newdata = pr_train1,type="response")
auc(roc(pr_train2$went_on_backorder,train2.score))
auc(roc(pr_train1$went_on_backorder,train1.score))

#train
train.score=predict(log_fit,newdata = pr_train,type = "response") #probabilty
auc(roc(pr_train$went_on_backorder,train.score))

#ks

real=pr_train$went_on_backorder
cutoffs=seq(.001,.999,.001)
length(cutoffs)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  print(paste0("cutoff is:",cutoff))
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
               
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))             
  
}

cutoff_data=cutoff_data[-1,]
View(cutoff_data)

#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()

## to see whr is ur maxx cutoff and KS

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
my_cutoff
View(cutoff_data) #look KS at cotoff=.006
#ks=.146

#score should be >.50
1-(0.025/.146) ##.83

## now that we have our cutoff we can convert score to hard classes
train.score
train.predicted=as.numeric(train.score>my_cutoff)
test.score=predict(log_fit,newdata = pr_test,type="response")
test.predicted=as.numeric(test.score>my_cutoff)

dim(pr_test)
length(test.predicted)
write.csv(test.predicted,"Priyanshi_Agarwal_P3_part2.csv",row.names = F)

#---------------------part1(quiz)----------------------------------------
#1
pr_train %>% 
  filter(went_on_backorder=="Yes") %>% 
  summarise(med1=median(perf_12_month_avg)-median(perf_6_month_avg))
#2            
chisq.test(pr_train$went_on_backorder,pr_train$deck_risk)
#3
sort(table(pr_train$pieces_past_due),decreasing = T)[1:5]
length(pr_train$pieces_past_due)
#4 factor

#5 mytry= square root of feature variable 
sqrt(30)

#6 rm
#7
cor(pr_train$forecast_9_month,pr_train$sales_9_month)
#8
pr_train %>% 
  group_by(went_on_backorder) %>% 
  summarise(sum=sum(min_bank))

#9 geom_histogram()
#10 color
##----------------------------------------------------------------------------
