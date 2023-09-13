
#Real_Estate_2


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
  mutate(type_h=as.numeric(Type=="h"),
         type_u=as.numeric(Type=="u")) %>% 
  select(-Type)

glimpse(hs_all)

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
  mutate(method_s=as.numeric(Method=="S"))%>% 
  select(-Method)
glimpse(hs_all)

Seller=table(hs_all$SellerG)
sort(Seller)

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

hs_all=CreateDummies(hs_all ,"SellerG",50)
glimpse(hs_all)


Council=table(hs_all$CouncilArea)
sort(Council)

hs_all=hs_all %>%
  mutate(CA_Banyule=as.numeric(CouncilArea=="Banyule"),
         CA_Bayside=as.numeric(CouncilArea=="Bayside"),
         CA_Boroondara=as.numeric(CouncilArea=="Boroondara"),
         CA_Brimbank=as.numeric(CouncilArea=="Brimbank"),
         CA_Darebin=as.numeric(CouncilArea=="Darebin"),
         CA_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
         CA_Monash=as.numeric(CouncilArea=="Monash"),
         CA_Melbourne=as.numeric(CouncilArea=="Melbourne"),
         CA_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
         CA_Manningham=as.numeric(CouncilArea=="Manningham"),
         CA_Kingston=as.numeric(CouncilArea=="Kingston"),
         CA_Hume=as.numeric(CouncilArea=="Hume"),
         CA_HobsonsB=as.numeric(CouncilArea=="Hobsons Bay"),
         CA_MoonValley=as.numeric(CouncilArea=="Moonee Valley"),
         CA_Moreland=as.numeric(CouncilArea=="Moreland"),
         CA_PortP=as.numeric(CouncilArea=="Port Phillip"),
         CA_Stonnington=as.numeric(CouncilArea=="Stonnington"),
         CA_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
         CA_Yarra=as.numeric(CouncilArea=="Yarra")) %>% 
  select(-CouncilArea)
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
s=sample(1:nrow(hs_train),0.8*nrow(hs_train))
hs_train1=hs_train[s,]
hs_train2=hs_train[-s,]

fit=lm(Price~.-Postcode,data=hs_train1)
library(car)
sort(vif(fit),decreasing = T)
fit=lm(Price~.-Postcode-sub_3,data=hs_train1)
sort(vif(fit),decreasing = T)
summary(fit)

fit=step(fit)

formula(fit)

summary(fit)

fit=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
         Car + Landsize + BuildingArea + YearBuilt + type_h + type_u + 
         sub_1 + sub_2 + sub_5 + sub_6 + sub_7 + sub_8 + sub_9 + sub_10 + 
         sub_11 + sub_12 + sub_13 + sub_14 + sub_15 + sub_16 + method_s + 
         SellerG_Raine + SellerG_Douglas + SellerG_Kay + SellerG_McGrath + 
         SellerG_Sweeney + SellerG_RT + 
         SellerG_Biggin + SellerG_Buxton + SellerG_Marshall + SellerG_Jellis + SellerG_Nelson + 
         CA_Banyule + CA_Boroondara + CA_Brimbank + CA_Darebin + CA_Glen_Eira + 
         CA_Maribyrnong + CA_HobsonsB + CA_MoonValley + CA_Moreland + 
         CA_PortP + CA_Stonnington, data = hs_train1)

summary (fit)

val.pred =predict(fit,newdata=hs_train2)
errors=hs_train2$Price-val.pred
errors**2%>% mean()%>% sqrt()

Predicted_hs_train_2_1=predict(fit,newdata = hs_train2)
Predicted_hs_train_2_1=round(Predicted_hs_train_2_1,1)
class(Predicted_hs_train_2_1)


plot(hs_train2$Price,Predicted_hs_train_2_1)

library(ggplot2)
d=data.frame(real=hs_train2$Price,predicted=Predicted_hs_train_2_1)
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
write.csv(hs_test,"Predicted_Prices_on_test_data_2.csv",row.names=F)
