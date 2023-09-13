


setwd("C:/Users/hp/OneDrive/Desktop/Data")


retail_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
retail_test=read.csv("store_test.csv",stringsAsFactors = FALSE)

library(dplyr)
glimpse(retail_train)

retail_test$store=NA

retail_train$data='train'
retail_test$data='test'

retail_all=rbind(retail_train,retail_test)

glimpse(retail_all)

table(retail_all$country)

table(retail_all$State)

retail_all$store=as.factor(retail_all$store)

glimpse(retail_all)

names(retail_all)[sapply(retail_all,function(x) is.character(x))]

table(retail_all$countyname)

table(retail_all$Areaname)

retail_all=retail_all%>% select(-countyname,-storecode,-Areaname,-countytownname)

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

for(col in names(retail_all)){
  if(sum(is.na(retail_all[,col]))>0 & !(col %in% c("data","store"))){
    retail_all[is.na(retail_all[,col]),col]=mean(retail_all[retail_all$data=='train',col],na.rm=T)
  }
}

glimpse(retail_all)

sum(sapply(retail_all,function(x) is.character(x)))

retail_train=retail_all %>% filter(data=='train') %>% select(-data)
retail_test=retail_all %>% filter(data=='test') %>% select (-data,-store)

set.seed(2)
s=sample(1:nrow(retail_train),0.8*nrow(retail_train))
retail_train1=retail_train[s,]
retail_train2=retail_train[-s,]

library(cvTools)

retail_train$store=as.factor(retail_train$store)
glimpse(retail_train)

param=list(mtry=c(3,4,6,8,10),
           ntree=c(50,100,200,500,700,800,900), 
           maxnodes=c(5,10,15,20,30,50,100,300,500,600,700),
           nodesize=c(1,2,5,10,20,30,40)       
)



mycost_auc=function(store,yhat){  
  roccurve=pROC::roc(store,yhat)
  score=pROC::auc(roccurve)
  return(score)
}  


subset_paras=function(full_list_para,n=10)
  
  {  
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trial=100
my_params=subset_paras(param,num_trial)
my_params

myauc=0

library(randomForest)
for(i in 1:num_trial)
  {  

  params=my_params[i,]
  
  k=cvTuning(randomForest,
             store~.-Id, 
             data =retail_train,
             tuning =params,
             folds = cvFolds(nrow(retail_train), K=5, type ="random"),
             cost =mycost_auc, 
             seed =2,
             predictArgs = list(type="prob"))
  
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    myauc=score.this
    best_params=params
  }
 
}

myauc


best_params


ci.rf.final=randomForest(store~.-Id,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=retail_train
)


test.score_final=predict(ci.rf.final,newdata=retail_test, type="prob")[,2]
write.csv(test.score_final,'Retail_Store_Prediction.csv',row.names = F)

test.score_final

p=importance(ci.rf.final)
p=as.data.frame(p)
p$VariableNames=rownames(p)
p %>% arrange(desc(MeanDecreaseGini))

varImpPlot(ci.rf.final)
