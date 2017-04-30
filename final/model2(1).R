#use aic variables on champion model
#without interaction
nameofAIC = names(model.AIC$coefficients)[-1]
table2 = arr.data %>% 
  group_by(championID) %>% 
  summarise(K = mean(K), D = mean(D), A = mean(A),
            dam = mean(dam), dToC = mean(dToC), lvl = mean(lvl), 
            damToken = mean(damToken), heal = mean(heal), minionKill = mean(minionKill),
            neutralMinionsKilled = mean(neutralMinionsKilled), goldEarned = mean(goldEarned),
            averageTimeCrowdControlDealt = mean(averageTimeCrowdControlDealt)
  )
wt<-arr.data%>%filter(teamID==100&lane==" TOP")
wm<-arr.data%>%filter(teamID==100&lane==" MIDDLE")
wj<-arr.data%>%filter(teamID==100&lane==" JUNGLE")
wc<-arr.data%>%filter(teamID==100&lane==" BOTTOM"&role==" DUO_CARRY")
ws<-arr.data%>%filter(teamID==100&lane==" BOTTOM"&role==" DUO_SUPPORT")
t<-arr.data%>%filter(teamID==200&lane==" TOP")
m<-arr.data%>%filter(teamID==200&lane==" MIDDLE")
j<-arr.data%>%filter(teamID==200&lane==" JUNGLE")
c<-arr.data%>%filter(teamID==200&lane==" BOTTOM"&role==" DUO_CARRY")
s<-arr.data%>%filter(teamID==200&lane==" BOTTOM"&role==" DUO_SUPPORT")
wt1<-wt%>%select(championID)%>%left_join(table2)
wm1<-wm%>%select(championID)%>%left_join(table2)
wj1<-wj%>%select(championID)%>%left_join(table2)
wc1<-wc%>%select(championID)%>%left_join(table2)
ws1<-ws%>%select(championID)%>%left_join(table2)
t1<-t%>%select(championID)%>%left_join(table2)
m1<-m%>%select(championID)%>%left_join(table2)
j1<-j%>%select(championID)%>%left_join(table2)
c1<-c%>%select(championID)%>%left_join(table2)
s1<-s%>%select(championID)%>%left_join(table2)
T<-(wt1-t1)[,2:13]
M<-(wm1-m1)[,2:13]
J<-(wj1-j1)[,2:13]
C<-(wc1-c1)[,2:13]
S<-(ws1-s1)[,2:13]
V<-cbind(wt$Winner,T,M,J,C,S)
write.csv(V,"V.csv")
V = read.csv("V.csv")
X = V[,nameofAIC]
Y = V$wt.Winner
n = dim(V)[1]
train = sample(n,floor(n*0.7))
train_x<-X[train,]
train_x = apply(train_x,2,scale)
train_y<-Y[train]
test_x<-X[-train,]
test_x = apply(test_x,2,scale)
test_y<-Y[-train]
#################################


#1
train_y<-as.factor(train_y)
test_y<-as.factor(test_y)
m6.1<-randomForest(x=train_x,y=train_y)
p6.1<-predict(m6.1,test_x,type="response")
CrossTable(test_y,p6.1)

#2
m6.2<-C5.0(x=train_x,y=train_y,trials = 100)
p6.2<-predict(m6.2,test_x,type="class")
CrossTable(test_y,p6.2)
#
#3
train_x<-as_tibble(train_x)
test_x = as_tibble(test_x)
test_y = as.numeric(test_y) -1
m6.3 = glm(train_y~.,data = train_x,family = binomial())
p3 = predict(m6.3,test_x,type="response")
p6.3 = (p6.3>=0.5)
r6.3 = 1-sum(abs(test_y-p6.3))/length(test_y)
#
summary(m6.3)

#########################
#model with interaction
champ.list = read.csv("champlist.csv")
ID_list = sort(unique(champ.list$championID))
for(i in 1:dim(champ.list)[1]){
  k = which(ID_list == champ.list$championID[i])
  champ.list$championID[i] = k
}
champ.list = champ.list %>%
  arrange(championID)
full.data = arr.data %>%
  left_join(champ.list,by="championID")
n = dim(full.data)[1]
odd.n = seq(1,n-1,2)
even.n = seq(2,n,2)
temp = full.data$Subclass.y[odd.n]
full.data$Subclass.y[odd.n] = full.data$Subclass.y[even.n]
full.data$Subclass.y[even.n] = temp
table3 = full.data %>%
  group_by(championID,Subclass.y) %>%
  summarise(
    K = mean(K), D = mean(D), A = mean(A),
    dam = mean(dam), dToC = mean(dToC), lvl = mean(lvl), 
    damToken = mean(damToken), heal = mean(heal), minionKill = mean(minionKill),
    neutralMinionsKilled = mean(neutralMinionsKilled), goldEarned = mean(goldEarned),
    averageTimeCrowdControlDealt = mean(averageTimeCrowdControlDealt)
  )
wt<-full.data%>%filter(teamID==100&lane==" TOP")
wm<-full.data%>%filter(teamID==100&lane==" MIDDLE")
wj<-full.data%>%filter(teamID==100&lane==" JUNGLE")
wc<-full.data%>%filter(teamID==100&lane==" BOTTOM"&role==" DUO_CARRY")
ws<-full.data%>%filter(teamID==100&lane==" BOTTOM"&role==" DUO_SUPPORT")
t<-full.data%>%filter(teamID==200&lane==" TOP")
m<-full.data%>%filter(teamID==200&lane==" MIDDLE")
j<-full.data%>%filter(teamID==200&lane==" JUNGLE")
c<-full.data%>%filter(teamID==200&lane==" BOTTOM"&role==" DUO_CARRY")
s<-full.data%>%filter(teamID==200&lane==" BOTTOM"&role==" DUO_SUPPORT")
wt2<-wt%>%select(championID,Subclass.y)%>%left_join(table3)
wm2<-wm%>%select(championID,Subclass.y)%>%left_join(table3)
wj2<-wj%>%select(championID,Subclass.y)%>%left_join(table3)
wc2<-wc%>%select(championID,Subclass.y)%>%left_join(table3)
ws2<-ws%>%select(championID,Subclass.y)%>%left_join(table3)
t2<-t%>%select(championID,Subclass.y)%>%left_join(table3)
m2<-m%>%select(championID,Subclass.y)%>%left_join(table3)
j2<-j%>%select(championID,Subclass.y)%>%left_join(table3)
c2<-c%>%select(championID,Subclass.y)%>%left_join(table3)
s2<-s%>%select(championID,Subclass.y)%>%left_join(table3)
T<-(wt2-t2)[,3:14]
M<-(wm2-m2)[,3:14]
J<-(wj2-j2)[,3:14]
C<-(wc2-c2)[,3:14]
S<-(ws2-s2)[,3:14]
V<-cbind(wt$Winner,T,M,J,C,S)
write.csv(V,"V.csv")
V = read.csv("V.csv")
X = V[,nameofAIC]
Y = V$wt.Winner
n = dim(V)[1]
train = sample(n,floor(n*0.7))
train_x<-X[train,]
train_x = apply(train_x,2,scale)
train_y<-Y[train]
test_x<-X[-train,]
test_x = apply(test_x,2,scale)
test_y<-Y[-train]

#1
train_y<-as.factor(train_y)
test_y<-as.factor(test_y)
m7.1<-randomForest(x=train_x,y=train_y)
p7.1<-predict(m7.1,test_x,type="response")
CrossTable(test_y,p7.1)
#
#2
m7.2<-C5.0(x=train_x,y=train_y,trials = 100)
p7.2<-predict(m7.2,test_x,type="class")
CrossTable(test_y,p7.2)
#
#3
train_x<-as_tibble(train_x)
test_x = as_tibble(test_x)
test_y = as.numeric(test_y) -1
m7.3 = glm(train_y~.,data = train_x,family = binomial())
p7.3 = predict(m7.3,test_x,type="response")
p7.3 = (p7.3>=0.5)
r7.3 = 1-sum(abs(test_y-p7.3))/length(test_y)
summary(m7.3)


