
setwd("C:/Users/hp/OneDrive/Desktop/Data")
hs_train=read.csv("housing_train.csv",stringsAsFactors = F)

hs_test= read.csv("housing_test.csv",stringsAsFactors = F)

hs_test$Price=NA

hs_train$data='train'
hs_test$data='test'

hs_all=rbind(hs_train,hs_test)

library(dplyr)
glimpse(hs_all)

hs_all= hs_all%>% 
  select(-Address)
glimpse(hs_all)

table(hs_all$Type)

hs_all=hs_all%>% 
  mutate(type_h=as.numeric(Type=="h")) %>% 
  select(-Type)

round(tapply(hs_all$Price,hs_all$Suburb,mean,na.rm=T))

hs_all=hs_all %>% 
  mutate(
    sub_1=as.numeric(Suburb%in%c("Campbellfield","Jacana")),
    sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    sub_16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))
  ) %>% 
  
  select(-Suburb)
glimpse(hs_all)

table(hs_all$Method)

hs_all=hs_all %>%
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_S=as.numeric(Method=="S"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB")) %>% 
  select(-Method)


glimpse(hs_all)

Seller=table(hs_all$SellerG)
sort(Seller)

Council=table(hs_all$CouncilArea)
sort(Council)

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
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


glimpse(hs_all)

hs_all=CreateDummies(hs_all ,"SellerG",100)
hs_all=CreateDummies(hs_all,"CouncilArea",100)

glimpse(hs_all)

lapply(hs_all,function(x) sum(is.na(x)))

hs_all=hs_all[!(is.na(hs_all$Postcode)),]

for(col in names(hs_all)){
  
  if(sum(is.na(hs_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    hs_all[is.na(hs_all[,col]),col]=mean(hs_all[,col],na.rm=T)
  }
  
}

glimpse(hs_all)
View(hs_all)

hs_train=hs_all %>% filter(data=='train') %>% select(-data)
hs_test=hs_all%>% filter(data=='test') %>% select(-data,-Price)

set.seed(2)
s=sample(1:nrow(hs_train),0.7*nrow(hs_train))
hs_train1=hs_train[s,]
hs_train2=hs_train[-s,]


fit=lm(Price~.-Postcode,data=hs_train1)

library(car)

sort(vif(fit),decreasing = T)

fit=lm(Price~.-Postcode-Method_S,data=hs_train1)

sort(vif(fit),decreasing = T)

fit=lm(Price~.-Postcode-Method_S-sub_3-CouncilArea_,data=hs_train1)

sort(vif(fit),decreasing = T)

summary(fit)

fit=step(fit)

formula(fit)

summary(fit)

fit=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
     Car + Landsize + BuildingArea + YearBuilt + type_h + 
     sub_2 + sub_5 + sub_6 + sub_7 + sub_8 + sub_9 + sub_10 + 
     sub_11 + sub_12 + sub_13 + sub_14 + sub_15 + sub_16 + Method_PI + 
     Method_SP + Method_VB + SellerG_Kay + SellerG_McGrath + SellerG_RT + 
     SellerG_Buxton + SellerG_Marshall + SellerG_Jellis + 
     CouncilArea_Manningham + CouncilArea_Brimbank + CouncilArea_HobsonsBay + 
     CouncilArea_Banyule + CouncilArea_PortPhillip + CouncilArea_Maribyrnong + 
     CouncilArea_GlenEira + CouncilArea_Darebin + CouncilArea_MooneeValley + 
     CouncilArea_Moreland, data = hs_train1)

summary (fit)

val.pred =predict(fit,newdata=hs_train2)
errors=hs_train2$Price-val.pred
errors**2%>% mean()%>% sqrt()

Predicted_hs_train_2=predict(fit,newdata = hs_train2)
Predicted_hs_train_2=round(Predicted_hs_train_2,1)
class(Predicted_hs_train_2)

plot(hs_train2$Price,Predicted_hs_train_2)

library(ggplot2)
d=data.frame(real=hs_train2$Price,predicted=Predicted_hs_train_2)
ggplot(d,aes(x=real,y=predicted))+geom_point()

plot(fit,which = 1)
plot(fit, which=2)
plot(fit, which=3)
plot(fit,which=4)


fit.final=fit=lm(Price~.-Postcode,data=hs_train)
fit.final=step(fit.final)

summary(fit.final)

test.pred=predict(fit.final,newdata = hs_train)

hs_test$test.pred=predict(fit.final,newdata = hs_test)
write.csv(hs_test,"Predicted_Prices_on_test_data.csv",row.names=F)
getwd()

plot(fit.final, which=1)
plot(fit.final,which=2)
plot(fit.final, which=3)
plot(fit.final,which=4)
