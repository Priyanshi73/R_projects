getwd()
setwd("C:\\Users\\Priyanshi\\Downloads\\Data\\project_2")
store_test=read.csv("store_test.csv",stringsAsFactors = FALSE)
store_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
View(store_train)
table(store_train$store)

dim(store_test)
dim(store_train)

##--------------------part1 (quiz)----------------------------

store_train %>% 
  filter(Areaname=="Kennebec County, ME") %>% 
  filter(store_Type=="Supermarket Type1") %>% 
  summarise("sum"=sum(sales0,sales1,sales2,sales3,sales4))


sort(table(store_train$country),decreasing = T)[1:5]

length(unique(store_train$Areaname))

table(store_train$store_Type)

store_train %>% 
  filter(store_Type=="Grocery Store") %>% 
  filter(store==1) %>% 
  summarise("len"=length(store))
#182/432

library(ggplot2)

ggplot(store_train,aes(x=sales0))+geom_histogram()
ggplot(store_train,aes(x=sales1))+geom_histogram()
ggplot(store_train,aes(x=sales2))+geom_histogram()
ggplot(store_train,aes(x=sales3))+geom_histogram()

table(store_train$store_Type)
store_train$total_sum=rowSums(store_train[,c("sales0","sales1","sales2","sales3","sales4")])

store_train %>% 
  group_by(store_Type) %>% 
  summarise("sum"=sum(sales0,sales1,sales2,sales3,sales4),
            "var"=var(total_sum)) 

total_sum=rowSums(store_train[,c("sales0","sales1","sales2","sales3","sales4")])
quantile(total_sum,.25)-1.5*IQR(total_sum) 
sum(total_sum<1101.875)
quantile(total_sum,.75)+1.5*IQR(total_sum) 
sum(total_sum>7288.875)

sort(table(store_train$state_alpha),decreasing = T)
length(unique(store_train$state_alpha))

--------------------------part2(code)----------------------------------------
setdiff(names(store_train),names(store_test))
store_test$store=NA

store_test$data='test'
store_train$data='train'

store_bind=rbind(store_train,store_test)

library(dplyr)
glimpse(store_bind)
dim(store_bind)
# 4769*.05=238.4 #for cutoff
# 4769 *.02=95.38

#------------------------------
#processing storecode
sort(table(store_bind$storecode),decreasing = T)[1:10]
table(substr(store_bind$storecode,1,5)) #as there are only 2 variable if we differentiate acc to first 5 alphabets so we will split it into "metro" and "ncnty"

store_bind=store_bind %>% 
  mutate(storecode_ncnty=as.numeric(substr(store_bind$storecode,1,5)=="NCNTY")) %>% 
  select(-storecode)
#-------------------------------------
#drop variables that have more unique values

table(store_bind$store_Type) #dummy encoding
sort(table(store_bind$state_alpha),decreasing = T)#dummy encoding
sort(table(store_bind$population),decreasing = T)[1:5] #as it is
unique(store_bind$population)
#-------------------------------------
sort(table(store_bind$countytownname),decreasing = T)[1:5]#drop
unique(store_bind$countytownname) #many unique values
sort(table(store_bind$Areaname),decreasing = T)[1:5]#drop
unique(store_bind$Areaname)#many unique values

store_bind=store_bind %>% 
  select(-countytownname,-Areaname)
  
#----------------------------------------------
glimpse(store_bind)
#-------------------------------------
#county name
sort(table(store_bind$countyname),decreasing = T)[1:10]
unique(store_bind$countyname)
store_bind=store_bind %>% 
  mutate(county_Washington=as.numeric(countyname=="Washington County")) %>% 
  select(-countyname)
##----------------------------------
#cousub
sort(table(store_bind$CouSub),decreasing = T)[1:5]
store_bind=store_bind %>% 
  mutate(cousub_99999=as.numeric(CouSub==99999)) %>% 
  select(-CouSub)
#---------------------------------
  
sort(table(store_bind$State),decreasing = T)[1:5]#as it is
unique(store_bind$State)
sort(table(store_bind$country),decreasing = T)[1:5]#as it is
unique(store_bind$country)
sort(table(store_bind$sales0),decreasing = T)#as it is
sort(table(store_bind$sales1),decreasing = T)#as it is
sort(table(store_bind$sales2),decreasing = T)#as it is
sort(table(store_bind$sales3),decreasing = T)#as it is
sort(table(store_bind$sales4),decreasing = T)#as it is
#----------------------------------
#dummy encoding

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])#frequency
  t=t[t>freq_cutoff] ## which are the categories to be made
  t=sort(t) ## sort them
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
store_bind=CreateDummies(store_bind,"store_Type",490)
store_bind=CreateDummies(store_bind,"state_alpha",250)
glimpse(store_bind)

#checking for NA
#or names(store_bind)[colSums(is.na(store_bind))>0]
colSums(is.na(store_bind))

#na_remove <- c("Bedroom2","Bathroom","Car","Landsize","BuildingArea","YearBuilt")

#rs[,na_remove] <- lapply(rs[,na_remove],function(x)ifelse(is.na(x),mean(x,na.rm=T),x))

na_remove=c("country","population")
store_bind[,na_remove] <- lapply(store_bind[,na_remove], function(x)ifelse(is.na(x),mean(x,na.rm=T),x))

#checking if any character variables
names(store_bind)[sapply(store_bind, function(x)is.character(x))]

#checking target variable
table(store_bind$store) #already in 1/0 format

store_train=store_bind %>% filter(data=="train") %>% select(-data)
store_test=store_bind %>% filter(data=="test") %>% select(-data,-store)
#---------------------------------------------------
glimpse(store_train)
glimpse(store_test)

set.seed(2)
s=sample(1:nrow(store_train),0.8*nrow(store_train))
store_train1=store_train[s,]
store_train2=store_train[-s,]

dim(store_train)
dim(store_train1)
dim(store_train2)

library(car)

fit=lm(store~.-Id,data=store_train1)
sort(vif(fit),decreasing = T)[1:3]

fit=lm(store~.-Id-sales0-sales2-sales3-cousub_99999-sales1,data=store_train1)
sort(vif(fit),decreasing = T)[1:3]

#glm
log_fit=glm(store~.-Id-sales0-sales2-sales3-cousub_99999-sales1,data=store_train1,family = "binomial")
log_fit=step(log_fit)
formula(log_fit)

log_fit=glm(store ~ storecode_ncnty,data=store_train,family = "binomial")
summary(log_fit)

#### performance of score model on validation data
library(pROC)
train2.score=predict(log_fit,newdata=store_train2,type = "response")
train1.score=predict(log_fit,newdata=store_train1,type="response")
length(train2.score)
length(store_train2$store)
auc(roc(store_train2$store,train2.score))
auc(roc(store_train1$store,train1.score))

#now checking and validating on train and test

train.score=predict(log_fit,newdata=store_train,type="response")
auc(roc(store_train$store,train.score))
test.score=predict(log_fit,newdata=store_test,type="response")
write.csv(test.score,"Priyanshi_Agarwal_P2_part2.csv",row.names = F)
