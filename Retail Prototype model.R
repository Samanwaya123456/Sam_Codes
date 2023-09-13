


#Retail_store

setwd("C:/Users/hp/OneDrive/Desktop/Data")


retail_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
retail_test=read.csv("store_test.csv",stringsAsFactors = FALSE)

library(dplyr)
glimpse(retail_train)

retail_test$store=NA

retail_train$data='train'
retail_test$data='test'

retail_all=rbind(retail_train,retail_test)

cn=table(retail_all$countyname)
sort(cn)

sum(unique(table(retail_all$countyname)))

sum(unique(table(retail_all$state_alpha)))

sum(unique(table(retail_all$Areaname)))

sum(unique(table(retail_all$countytownname)))

length(unique(retail_all$state_alpha))

lapply(retail_all,function(x) length(unique(x)))

names(retail_all)[sapply(retail_all,function(x) is.character(x))]

retail_all=retail_all %>% select(-countyname,-storecode,-Areaname,-countytownname)

glimpse(retail_all)

table(retail_all$state_alpha)

table(retail_all$store_Type)

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


cat_cols=c("state_alpha","store_Type")

for(cat in cat_cols){
  retail_all=CreateDummies(retail_all,cat,50) 
}

glimpse(retail_all)

lapply(retail_all,function(x) sum(is.na(x)))

retail_all=retail_all[!((is.na(retail_all$store)) & retail_all$data=='train'), ]
for(col in names(retail_all)){
  if(sum(is.na(retail_all[,col]))>0 & !(col %in% c("data","store"))){
    retail_all[is.na(retail_all[,col]),col]=mean(retail_all[retail_all$data=='train',col],na.rm=T)
  }
}

colSums(is.na(retail_all))

glimpse(retail_all)

sum(sapply(retail_all,function(x) is.character(x)))

retail_train=retail_all %>% filter(data=='train') %>% select(-data)
retail_test=retail_all %>% filter(data=='test') %>% select (-data,-store)

set.seed(2)
s=sample(1:nrow(retail_train),0.8*nrow(retail_train))
retail_train1=retail_train[s,]
retail_train2=retail_train[-s,]

library(car)

for_vif=lm(store~.-Id,data=retail_train1)

sort(vif(for_vif),decreasing = T)

for_vif=lm(store~.-Id-sales0,data=retail_train1)

sort(vif(for_vif),decreasing = T)

for_vif=lm(store~.-Id-sales0-sales2,data=retail_train1)

sort(vif(for_vif),decreasing = T)

for_vif=lm(store~.-Id-sales0-sales2-sales3,data=retail_train1)

sort(vif(for_vif),decreasing = T)

summary(for_vif)

log_fit=glm(store~.-Id-sales0-sales2-sales3,data=retail_train1,family = "binomial")

log_fit=step(log_fit)

formula(log_fit)

log_fit=glm(store~ sales4 + State + population + state_alpha_WV + state_alpha_LA + 
              state_alpha_AL + state_alpha_AR + state_alpha_PR + state_alpha_IN + 
              state_alpha_TN + state_alpha_IL + state_alpha_MO + state_alpha_KY + 
              state_alpha_GA + state_alpha_CT + state_alpha_VT + state_alpha_NH + 
              state_alpha_MA + state_alpha_ME,data=retail_train1,family="binomial")
summary(log_fit)

log_fit=glm(store~ sales4 + State + population + state_alpha_WV + state_alpha_LA + 
              state_alpha_AL + state_alpha_PR + state_alpha_IN + 
              state_alpha_TN + state_alpha_IL + state_alpha_KY + 
              state_alpha_GA + state_alpha_CT + state_alpha_VT + state_alpha_NH + 
              state_alpha_MA + state_alpha_ME,data=retail_train1,family="binomial")

summary(log_fit)

library(pROC)

val.score=predict(log_fit,newdata = retail_train2,type='response')

auc(roc(retail_train2$store,val.score))
plot(roc(retail_train2$store,val.score))

library(tree)

Dt_retail= tree(as.factor(store)~.-Id,data=retail_train1)

DT_retail_score=predict(Dt_retail,newdata=retail_train2,type="vector")[,2]
auc(roc(retail_train2$store,DT_retail_score))

library(randomForest)
rf.model= randomForest(as.factor(store)~.-Id,data=retail_train1)
test.score=predict(rf.model,newdata=retail_train2,type="prob")[,2]
auc(roc(retail_train2$store,test.score))
