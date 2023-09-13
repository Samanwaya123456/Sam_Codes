

#Banking



setwd("C:/Users/hp/OneDrive/Desktop/Data")

bank_train=read.csv("bank-full_train.csv",stringsAsFactors = FALSE)
bank_test=read.csv("bank-full_test.csv",stringsAsFactors = FALSE)

library(dplyr)
glimpse(bank_train)

bank_test$y=NA
bank_train$data='train'
bank_test$data='test'
bank_all=rbind(bank_train,bank_test)

glimpse(bank_all)

apply(bank_all,2,function(x)sum(is.na(x)))


a=table(bank_all$job)
sort(a)

job_dummy=round(prop.table(table(bank_all$job,bank_all$y),1)*100,1)
job_dummy



bank_all=bank_all %>% 
  mutate(job_1=as.numeric(job %in% c("self-employed","unknown","technician")), 
         job_2=as.numeric(job %in% c("services","housemaid","entrepreneur")),
         job_3=as.numeric(job %in% c("management","admin")),
         job_4=as.numeric(job=="student"),
         job_5=as.numeric(job=="retired"),
         job_6=as.numeric(job=="unemployed")) %>% 
  select(-job)

glimpse(bank_all)

m=table(bank_all$marital)
sort(m)

bank_all=bank_all %>% 
  mutate(married=as.numeric(marital %in% c("married")),
         single=as.numeric(marital %in% c("single"))
  ) %>% 
  select(-marital)
glimpse(bank_all)

e=table(bank_all$education)
sort(e)

bank_all=bank_all %>% 
  mutate(education_primary=as.numeric(education %in% c("primary")),
         education_secondary=as.numeric(education %in% c("secondary")),
         education_tertiary=as.numeric(education %in% c("tertiary"))
  ) %>% 
  select(-education)
glimpse(bank_all)

table(bank_all$default)


bank_all$default=as.numeric(bank_all$default=="yes")

glimpse(bank_all)


table(bank_all$loan)

bank_all$loan=as.numeric(bank_all$loan=="yes")
glimpse(bank_all)

table(bank_all$contact)

bank_all=bank_all %>% 
  mutate(contact_cellular=as.numeric(contact %in% c("cellular")),
         contact_telephone=as.numeric(contact %in% c("telephone"))
  ) %>% 
  select(-contact)
glimpse(bank_all)


table(bank_train$y)

bank_all$y=as.numeric(bank_all$y=="yes")
table(bank_all$y)


table(bank_all$month)

month_dummy=round(prop.table(table(bank_all$month,bank_all$y),1)*100,1)
month_dummy

bank_all=bank_all %>% 
  mutate(month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")) %>% 
  select(-month)
glimpse(bank_all)

table(bank_all$poutcome)

bank_all=bank_all %>% 
  mutate(poutcome_unknown=as.numeric(poutcome %in% c("unknown")),
         poc_failure=as.numeric(poutcome %in% c("failure")),
         poc_other=as.numeric(poutcome %in% c("other"))
  )%>% 
  select(-poutcome)
glimpse(bank_all)

table(bank_all$housing)
bank_all$housing=as.numeric(bank_all$housing=="yes")
glimpse(bank_all)

lapply(bank_all,function(x) sum(is.na(x)))

lapply(bank_all,function(x) sum(is.na(x)))

for(col in names(bank_all)){
  
  if(sum(is.na(bank_all[,col]))>0 & !(col %in% c("data","y"))){
    
    bank_all[is.na(bank_all[,col]),col]=mean(bank_all[bank_all$data=='train',col],na.rm=T)
  }
  
}

lapply(bank_all,function(x) sum(is.na(x)))

bank_train=bank_all %>% filter(data=='train') %>% select(-data)
bank_test=bank_all %>% filter(data=='test') %>% select (-data,-y)

glimpse(bank_train)
glimpse(bank_test)

set.seed(2)
s=sample(1:nrow(bank_train),0.8*nrow(bank_train))
bank_train1=bank_train[s,]
bank_train2=bank_train[-s,]


library(car)

for_vif=lm(y~.-ID,data=bank_train1)

sort(vif(for_vif),decreasing = T)

summary(for_vif)

log_fit=glm(y~.-ID,data=bank_train1,family = "binomial")

log_fit=step(log_fit)
formula(log_fit)

log_fit=glm(y ~ balance + housing + loan + duration + campaign + previous + 
              job_2 + job_4 + job_5 + married + education_primary + education_tertiary + 
              contact_cellular + contact_telephone + month_2 + month_3 + 
              month_4 + month_5 + month_6 + poutcome_unknown + poc_failure + 
              poc_other, data=bank_train1,family='binomial')

summary(log_fit)

log_fit=glm(y ~ balance + housing + loan + duration + campaign + 
              job_2 + job_4 + job_5 + married + education_primary + education_tertiary + 
              contact_cellular + contact_telephone + month_2 + month_3 + 
              month_4 + month_5 + month_6 + poutcome_unknown + poc_failure + 
              poc_other, data=bank_train1,family='binomial')

summary(log_fit)

library(pROC)

val.score=predict(log_fit,newdata = bank_train2,type='response')

auc(roc(bank_train2$y,val.score))
plot(roc(bank_train2$y,val.score))


log.fit.final=glm(y~.-ID,data=bank_train1,family = "binomial")

log.fit.final=step(log.fit.final)

formula(log.fit.final)

log.fit.final=glm(y~ balance + housing + loan + duration + campaign + previous + 
                    job_2 + job_4 + job_5 + married + education_primary + education_tertiary + 
                    contact_cellular + contact_telephone + month_2 + month_3 + 
                    month_4 + month_5 + month_6 + poutcome_unknown + poc_failure + 
                    poc_other,
                  data=bank_train,family='binomial')


summary(log.fit.final)

bank_test$test.prob.score= predict(log.fit.final,newdata = bank_test,type='response')
write.csv(bank_test,"Banking_Predictions.csv",row.names = F)
glimpse(bank_test)

train.score=predict(log.fit.final,newdata = bank_train,type="response")
real=bank_train$y
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)


for(cutoff in cutoffs){
  
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

library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

bank_test$test.predicted=as.numeric(bank_test$test.prob.score>my_cutoff)
write.csv(rg_test,"Revenue_grid_predictions.csv",row.names = F)
