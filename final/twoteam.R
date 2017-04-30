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

raw.data = read.csv("new_game_data.csv", head=T)
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

table = temp.data %>% 
  group_by(championID) %>% 
  summarise(kill = mean(K), death = mean(D), assit = mean(A),
            dam = mean(dam), dToC = mean(dToC), lv = mean(lvl), 
            damToken = mean(damToken), heal = mean(heal), minionKill = mean(minionKill),
            neutralMinionsKilled = mean(neutralMinionsKilled), gold = mean(goldEarned),
            averageTimeCrowdControlDealt = mean(averageTimeCrowdControlDealt)
  )




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
#pick good game
team1<-rep(c(rep(TRUE,5),rep(FALSE,5)),dim(temp.data1)[1]/10)
team2<-rep(c(rep(FALSE,5),rep(TRUE,5)),dim(temp.data1)[1]/10)
T1<-temp.data1[team1,]
T2<-temp.data1[team2,]
wt<-T1%>%filter(lane==" TOP")
wm<-T1%>%filter(lane==" MIDDLE")
wj<-T1%>%filter(lane==" JUNGLE")
wb<-T1%>%filter(lane==" BOTTOM")
t<-T2%>%filter(lane==" TOP")
m<-T2%>%filter(lane==" MIDDLE")
j<-T2%>%filter(lane==" JUNGLE")
b<-T2%>%filter(lane==" BOTTOM")
wt1<-wt%>%select(Winner,championID)%>%left_join(table)
wm1<-wm%>%select(Winner,championID)%>%left_join(table)
wj1<-wj%>%select(Winner,championID)%>%left_join(table)
wb1<-wb%>%select(Winner,championID)%>%left_join(table)
t1<-t%>%select(Winner,championID)%>%left_join(table)
m1<-m%>%select(Winner,championID)%>%left_join(table)
j1<-j%>%select(Winner,championID)%>%left_join(table)
b1<-b%>%select(Winner,championID)%>%left_join(table)
T<-cbind(wt1[,1],(wt1[,3:14]-t1[,3:14]))
M<-cbind(wm1[,1],(wm1[,3:14]-m1[,3:14]))
J<-cbind(wj1[,1],(wj1[,3:14]-j1[,3:14]))
odd<-seq(1,dim(T)[1]*2,2)
even<-seq(2,dim(T)[1]*2,2)
B<-cbind(wb1[odd,1],(wb1[odd,3:14]+wb1[even,3:14]-b1[odd,3:14]-b1[even,3:14]))

all1<-T1%>%group_by(matchID)%>%summarise(dToC=sum(dToC),heal=sum(heal),gold=sum(goldEarned),cc=sum(averageTimeCrowdControlDealt))
all2<-T2%>%group_by(matchID)%>%summarise(dToC=sum(dToC),heal=sum(heal),gold=sum(goldEarned),cc=sum(averageTimeCrowdControlDealt))
All<-(all1-all2)[,2:5]
W<-T$Winner
T<-scale(T[,2:13])
M<-scale(M[,2:13])
J<-scale(J[,2:13])
B<-scale(B[,2:13])
All<-scale(All)
VX<-cbind(T,M,J,B,All)
VY<-as.factor(W)
VY<-as.numeric(VY)
write.csv(VX,"VX.csv")
write.csv(VY,"VY.csv")

train_x<-VX[1:7000,]
train_y<-VY[1:7000]
test_x<-VX[7001:10672,]
test_y<-VY[7001:10672]


m<-C5.0(x=train_x,y=train_y,trials = 100)
p<-predict(m,test_x,type="class")
CrossTable(test_y,p)#51.6%
C5imp(m)
plot(m)

m<-randomForest(x=train_x,y=train_y)
p<-predict(m,test_x,type="response")
CrossTable(test_y,p)#51.9%
varImpPlot(m)

m<-msgl::fit(x=train_x,train_y,alpha = 0.5,lambda = 0.7,grouping = c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,4)))
res<-predict(m,test_x)
Err(res,classes=test_y)#

train_y<-as.numeric(train_y)
test_y<-as.numeric(test_y)
train_y<-train_y-1
test_y<-test_y-1
dtrain <- xgb.DMatrix(train_x, label = train_y)
dtest <- xgb.DMatrix(test_x, label = test_y) 
watchlist <- list(eval = dtest, train = dtrain)
param <- list(subsample=0.7,eta=0.5,gamma=0.5,max_depth = 4, silent = 1, objective = "binary:logistic")
bst <- xgb.train(param, dtrain, nrounds = 100, watchlist)#84%
bst1<-xgb.importance(colnames(train_x),bst)#54.1%
xgb.plot.importance(bst1,rel_to_first =FALSE)

h2o.init()
V1$win<-as.factor(V1$win)
V3<-as.h2o(V1)
m<-h2o.deeplearning(x=2:53,y=1,training_frame = V3, standardize = FALSE,activation = "Tanh",hidden = 5,epochs = 200,
                    train_samples_per_iteration = 200,adaptive_rate = TRUE,
                    loss="CrossEntropy",distribution = "bernoulli",shuffle_training_data = TRUE)
pre<-h2o.predict(m,V3)

insvalue<-V1[,2:53]
instarget<-decodeClassLabels(V1[,1])%>%as.data.frame()
write_csv(insvalue,"x.csv")
write_csv(instarget,"y.csv")
ins<-splitForTrainingAndTest(insvalue, instarget, ratio=0.25)
m <- mlp(ins$inputsTrain, ins$targetsTrain, size=5, learnFuncParams=c(0.001), maxit=200, 
         inputsTest=ins$inputsTest, targetsTest=ins$targetsTest)
plotIterativeError(m)
predictions <- predict(m,ins$inputsTest)
confusionMatrix(ins$targetsTest,predictions)#53.5%
