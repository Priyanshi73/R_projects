getwd()
setwd("C:\\Users\\Priyanshi\\Downloads\\Data\\project")
library(dplyr)
rs_test <- read.csv("housing_test.csv",stringsAsFactors = FALSE)
rs_train <- read.csv("housing_train.csv",stringsAsFactors = FALSE)

dim(rs_test)
dim(rs_train)
setdiff(names(rs_train),names(rs_test))

## outliers for rooms

table(rs_train$Rooms)
#upper outlier
quantile(rs_train$Rooms,.75)+1.5*IQR(rs_train$Rooms)
sum(rs_train$Rooms>4.5) #421
#lower outlier
quantile(rs_train$Rooms,.25)-1.5*IQR(rs_train$Rooms)
sum(rs_train$Rooms<0.5)
dim(rs_train)
rs_train <- rs_train[rs_train$Rooms<4.5,]
glimpse(rs_train)
rs_test$Price =NA

rs_test$data ="test"
rs_train$data="train"
dim(rs_test)
dim(rs_train)
rs<- rbind(rs_train,rs_test)
dim(rs)
library("dplyr")
View(rs)
glimpse(rs)

#mean(housing_train$Price,na.rm = TRUE)
#median(housing_train$Price,na.rm = TRUE)
#upper outlier-- Q3 + 1.5 * IQR

sort(table(rs$Suburb),decreasing = T)
table(rs$Address)

#will remove address and suburb
rs =rs %>% 
  select(-c(Address,Suburb))
glimpse(rs)


#checking which have character values
names(rs)[sapply(rs,function(x)is.character(x))]

#character variables
table(rs$Type)
table(rs$Method)
sort(table(rs$SellerG),decreasing = T)

rs=rs %>% 
  mutate(SellerG_Nelson=as.numeric(SellerG=="Nelson"),
         SellerG_Jellis=as.numeric(SellerG=="Lellis")) %>% 
  select(-SellerG)

glimpse(rs)

sort(table(rs$CouncilArea),decreasing = T)
rs=rs %>% 
  mutate(CouncilArea=gsub('^$','Missing',trimws(CouncilArea)))

sum(is.na(rs$CouncilArea))

#dummy encoding

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
rs=CreateDummies(rs,"Type",100)
rs=CreateDummies(rs,"Method",100)
rs=CreateDummies(rs,"CouncilArea",600)
sort(table(rs$Distance),decreasing = T)
table(rs$Postcode)
table(rs$Bedroom2)
table(rs$Bathroom)
table(rs$Car)
table(rs$Landsize)
table(rs$BuildingArea)
table(rs$YearBuilt)
glimpse(rs)
colSums(is.na(rs))

na_remove <- c("Bedroom2","Bathroom","Car","Landsize","BuildingArea","YearBuilt")

rs[,na_remove] <- lapply(rs[,na_remove],function(x)ifelse(is.na(x),mean(x,na.rm=T),x))
colSums(is.na(rs))
dim(rs)
#separate test and train
rs_train=rs %>% filter(data=='train') %>% select(-data)
rs_test=rs %>% filter(data=='test') %>% select(-data,-Price)
dim(rs_train)[1]*.7

set.seed(2)
s=sample(1:nrow(rs_train),0.7*nrow(rs_train))
rs_train1=rs_train[s,] #70% data
rs_train2=rs_train[-s,] #30% data
dim(rs_train1)
dim(rs_train2)

glimpse(rs_train1)

#lm
fit=lm(Price~.-SellerG_Jellis,data=rs_train1)
library(car)
sort(vif(fit),decreasing = T)
alias_train <- alias(fit)
alias_train$Complete # remove watevr variables are there in it.. eg SellerG_Jellis in this
summary(fit)
fit=step(fit)
formula(fit)

fit=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
         Landsize + BuildingArea + YearBuilt + SellerG_Nelson + Type_u + 
         Type_h + Method_PI + Method_S + CouncilArea_Boroondara + 
         CouncilArea_Moreland,data=rs_train1)
summary(fit)
## validation on 30% data
val.pred_train1=predict(fit,newdata = rs_train1)
val.pred_train2=predict(fit,newdata = rs_train2)

error_train1=rs_train1$Price-val.pred_train1
error_train2=rs_train2$Price-val.pred_train2

rmse_rs_train1=error_train1**2 %>% mean() %>% sqrt()
rmse_rs_train2=error_train2**2 %>% mean() %>% sqrt()
 #validation
212467/rmse_rs_train1
212467/rmse_rs_train2

#for full train data-- validation on train data
final_fit=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
               Landsize + BuildingArea + YearBuilt + SellerG_Nelson + Type_u + 
               Type_h + Method_PI + Method_S + CouncilArea_Boroondara + 
               CouncilArea_Moreland,data=rs_train)
summary(final_fit)
val.pred_train=predict(final_fit,newdata = rs_train)
error_train=rs_train$Price-val.pred_train
rmse_rs_train=error_train**2 %>% mean() %>% sqrt()
212467/rmse_rs_train
#scoring on test data
val.pred_test=predict(final_fit,newdata = rs_test)
length(val.pred_test)
write.csv(val.pred_test,"Priyanshi_Agarwal_P1_part2.csv",row.names = F)

#---------------------------using random forest-------------------------------
library(randomForest)
#hyperparameter

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))
fparam <- expand.grid(param)
nrow(fparam) #600 combinations

#
subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=50 # 600 me se koi bhi 50 combinations
my_params=subset_paras(param,num_trials)
nrow(my_params) #can check
dim(my_params)
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 50
head(rs_train)

myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  params=my_params[i,]
  
  k=cvTuning(randomForest,Price~.,
             data =rs_train,
             tuning =params,
             folds = cvFolds(nrow(rs_train), K=10, type = "random"),
             seed =2
  )
  print(paste0("num trials",i))
  #print(k)
  #print(k$cv)
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

## from another run following values were obtained
myerror
best_params

212467/myerror
ld.rf.final=randomForest(Price~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=rs_train)

test.pred=predict(ld.rf.final,newdata = rs_test)
length(test.pred)
write.csv(test.pred,"Priyanshi_Agarwal_P1_part2.csv",row.names = F)

##--------------- for project -PART1- quizz---------------------------------

var(rs_train$Price)
sum(is.na(rs_train$YearBuilt))
table(rs_train$Type)
rs_train %>% 
  group_by(Type) %>% 
  summarise("mean"=mean(Price,na.rm = T))

length(unique(rs_train$Postcode))
table(rs_train$Distance)
hist(rs_train$Distance)
plot(density(rs_train$Distance))
install.packages("normtest")
library(normtest)
ad.test()

View(rs_train)
rs_train %>% 
  group_by(SellerG) %>% 
  summarise("summm"=sum(Price,na.rm = T)) %>% 
  arrange(desc(summm))

rs_train %>% 
  group_by(CouncilArea) %>% 
  summarise("mean_cc"=mean(Price,na.rm = T)) %>% 
  arrange(desc(mean_cc))

rs_train %>% 
  group_by(CouncilArea) %>% 
  summarise("var_cc"=var(Price,na.rm = T)) %>% 
  arrange(desc(var_cc))
##--------------------------------------------------------------------
