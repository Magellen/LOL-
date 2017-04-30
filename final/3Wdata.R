###transform the raw data

require(tidyverse)
require(glmnet)
require(randomForest)
require(C50)
require(gmodels)
require(xgboost)
require(boot)
require(msgl)
require(h2o)
require(RSNNS)
require(ggplot2)

raw.data = read.csv("match_data.csv", head=T)
raw.data = as_tibble(raw.data)
raw.data$gameLength = raw.data$gameLength/60
#note:change the unit from second into minute
raw.data$totalTimeCrowdControlDealt = raw.data$totalTimeCrowdControlDealt/5
colnames(raw.data)[25] = "averageTimeCrowdControlDealt"
#note:change the variable from total to average, which makes more sense
ID_list = sort(unique(raw.data$championID))
for(i in 1:dim(raw.data)[1]){
  k = which(ID_list == raw.data$championID[i])
  raw.data$championID[i] = k
}
#note:change the championID into 1:134, this is a stupid code, you guys can improve it

##Final type 
##divide by time and then center and scale according to position
k = c(17:25)
#note:variables to divide by time
temp.data = raw.data
temp.data[,k] = raw.data[,k]/unlist(raw.data[,26])
temp.data2 = temp.data %>% 
  group_by(lane,role) %>% 
  mutate(K = scale(K), D = scale(D), A = scale(A),
         dam = scale(dam), dToC = scale(dToC), lvl = scale(lvl), 
         damToken = scale(damToken), heal = scale(heal), minionKill = scale(minionKill),
         neutralMinionsKilled = scale(neutralMinionsKilled), gold = scale(goldEarned),
         averageTimeCrowdControlDealt = scale(averageTimeCrowdControlDealt)) 
table = temp.data2 %>% 
  group_by(championID) %>% 
  summarise(kill = mean(K), death = mean(D), assit = mean(A),
            dam = mean(dam), dToC = mean(dToC), lv = mean(lvl), 
            damToken = mean(damToken), heal = mean(heal), minionKill = mean(minionKill),
            neutralMinionsKilled = mean(neutralMinionsKilled), gold = mean(goldEarned),
            averageTimeCrowdControlDealt = mean(averageTimeCrowdControlDealt)
  )

##transfer raw data for model fitting
ID = c("ID1", "ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10")
m = dim(raw.data)[1]/10
new.data = raw.data %>% 
  select(matchID, championID, Winner, teamID) %>% 
  unite(win, teamID, Winner, sep="") %>%
  mutate(Winner = ((win=="100 True")|(win=="200 False")), type = rep(ID, m)) %>%
  select(matchID, championID, Winner, type) %>% 
  spread(key = type, value = championID) %>% 
  mutate(matchID = 1:m)
#try2
temp.data$Winner<-as.character(temp.data$Winner)
temp.data$lane<-as.character(temp.data$lane)
temp.data$role<-as.character(temp.data$role)
wt<-temp.data%>%filter(Winner==" True"&lane==" TOP")
wm<-temp.data%>%filter(Winner==" True"&lane==" MIDDLE")
wj<-temp.data%>%filter(Winner==" True"&lane==" JUNGLE")
wb<-temp.data%>%filter(Winner==" True"&lane==" BOTTOM")
t<-temp.data%>%filter(Winner!=" True"&lane==" TOP")
m<-temp.data%>%filter(Winner!=" True"&lane==" MIDDLE")
j<-temp.data%>%filter(Winner!=" True"&lane==" JUNGLE")
b<-temp.data%>%filter(Winner!=" True"&lane==" BOTTOM")
l1<-wt%>%count(matchID,Winner)%>%filter(n>1)%>%select(matchID)
l2<-t%>%count(matchID,Winner)%>%filter(n>1)%>%select(matchID)
l3<-wm%>%count(matchID,Winner)%>%filter(n>1)%>%select(matchID)
l4<-m%>%count(matchID,Winner)%>%filter(n>1)%>%select(matchID)
l5<-wj%>%count(matchID,Winner)%>%filter(n>1)%>%select(matchID)
l6<-j%>%count(matchID,Winner)%>%filter(n>1)%>%select(matchID)
l7<-wb%>%count(matchID,Winner)%>%filter(n!=2)%>%select(matchID)
l8<-b%>%count(matchID,Winner)%>%filter(n!=2)%>%select(matchID)
bad<-rbind(l1,l2,l3,l4,l5,l6,l7,l8)%>%unique()
good<-temp.data%>%group_by(matchID)%>%select(matchID)%>%setdiff(bad)
temp.data1<-temp.data%>%filter(matchID %in% good$matchID) #90% is good
wt<-temp.data1%>%filter(Winner==" True"&lane==" TOP")
wm<-temp.data1%>%filter(Winner==" True"&lane==" MIDDLE")
wj<-temp.data1%>%filter(Winner==" True"&lane==" JUNGLE")
wb<-temp.data1%>%filter(Winner==" True"&lane==" BOTTOM")
t<-temp.data1%>%filter(Winner!=" True"&lane==" TOP")
m<-temp.data1%>%filter(Winner!=" True"&lane==" MIDDLE")
j<-temp.data1%>%filter(Winner!=" True"&lane==" JUNGLE")
b<-temp.data1%>%filter(Winner!=" True"&lane==" BOTTOM")
#remove the abnormal comp, leave 1t1m1g2b
wt1<-wt%>%select(championID)%>%left_join(table)
wm1<-wm%>%select(championID)%>%left_join(table)
wj1<-wj%>%select(championID)%>%left_join(table)
wb1<-wb%>%select(championID)%>%left_join(table)
t1<-t%>%select(championID)%>%left_join(table)
m1<-m%>%select(championID)%>%left_join(table)
j1<-j%>%select(championID)%>%left_join(table)
b1<-b%>%select(championID)%>%left_join(table)
T<-(wt1-t1)[,2:13]
M<-(wm1-m1)[,2:13]
J<-(wj1-j1)[,2:13]
odd<-seq(1,dim(wt)[1]*2,2)
even<-seq(2,dim(wt)[1]*2,2)
B<-(wb1[odd,]+wb1[even,]-b1[odd,]-b1[even,])[,2:13]
all<-temp.data1%>%select(matchID,Winner,championID)%>%left_join(table)
all1<-all%>%group_by(matchID,Winner)%>%summarise(dToC=sum(dToC),heal=sum(heal),gold=sum(gold),cc=sum(averageTimeCrowdControlDealt))
all2<-all1%>%filter(Winner==" True")
all3<-all1%>%filter(Winner!=" True")
all2<-as.data.frame(all2[,3:6])
all3<-as.data.frame(all3[,3:6])
All<-all2-all3
#5 features
V<-cbind(T,M,J,B,All)
V<-scale(V)
V1<-data.frame(win=1,V)
set.seed(1237)
sub<-sample(1:dim(wt)[1],floor(dim(wt)[1]/2))
V1[sub,]<--V1[sub,]
V1[V1$win==-1,1]=0
write_csv(V1,"feature.csv")

V2<-as.matrix(V1)
train_x<-V2[1:30000,2:53]
train_y<-V2[1:30000,1]
test_x<-V2[30001:37220,2:53]
test_y<-V2[30001:37220,1]
train_y<-as.factor(train_y)
test_y<-as.factor(test_y)
m<-randomForest(x=train_x,y=train_y)
p<-predict(m,test_x,type="response")
CrossTable(test_y,p)#83
varImpPlot(m)