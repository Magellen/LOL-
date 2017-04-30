##match-based model

##model 1(simple one,sum of team1 minus sum of team2)
test.data = raw.data
n = dim(raw.data)[1]
for(i in 1:(n/10)){
  j = i*10
  test.data$K[j] = sum(test.data$K[(j-9):(j-5)]-test.data$K[(j-4):j])
  test.data$D[j] = sum(test.data$D[(j-9):(j-5)]-test.data$D[(j-4):j])
  test.data$A[j] = sum(test.data$A[(j-9):(j-5)]-test.data$A[(j-4):j])
  test.data$dam[j] = sum(test.data$dam[(j-9):(j-5)]-test.data$dam[(j-4):j])
  test.data$dToC[j] = sum(test.data$dToC[(j-9):(j-5)]-test.data$dToC[(j-4):j])
  test.data$lvl[j] = sum(test.data$lvl[(j-9):(j-5)]-test.data$lvl[(j-4):j])
  test.data$damToken[j] = sum(test.data$damToken[(j-9):(j-5)]-test.data$damToken[(j-4):j])
  test.data$heal[j] = sum(test.data$heal[(j-9):(j-5)]-test.data$heal[(j-4):j])
  test.data$minionKill[j] = sum(test.data$minionKill[(j-9):(j-5)]-test.data$minionKill[(j-4):j])
  test.data$neutralMinionsKilled[j] = sum(test.data$neutralMinionsKilled[(j-9):(j-5)]-test.data$neutralMinionsKilled[(j-4):j])
  test.data$goldEarned[j] = sum(test.data$goldEarned[(j-9):(j-5)]-test.data$goldEarned[(j-4):j])
  test.data$averageTimeCrowdControlDealt[j] = sum(test.data$averageTimeCrowdControlDealt[(j-9):(j-5)]-test.data$averageTimeCrowdControlDealt[(j-4):j])
}
m = seq(10,n,10)
r.data = test.data[m,c(7,14:25)]
r.data$Winner = (as.numeric(r.data$Winner)==1)
train = sample(n/10,floor(n*0.7/10))
tr.data = r.data[train,]
t.data = r.data[-train,]
#
#random froest
train_y<-as.factor(tr.data$Winner)
test_y<-as.factor(t.data$Winner)
m1.1<-randomForest(x=tr.data[,-1],y=train_y)
p1.1<-predict(m1.1,t.data[,-1],type="response")
CrossTable(test_y,p1.1)
#
#c5.0
train_y<-as.factor(tr.data$Winner)
test_y<-as.factor(t.data$Winner)
m1.2<-C5.0(x=tr.data[,-1],y=train_y,trials = 100)
p1.2<-predict(m1.2,t.data[,-1],type="class")
CrossTable(test_y,p1.2)
#
#glm
m1.3 = glm(train_y~.,data = tr.data[,-1],family = binomial())
p1.3 = predict(m1.3,t.data,type="response")
p1.3 = (p1.3>=0.5)
y = as.numeric(test_y) - 1
r3 = 1-sum(abs(y-p1.3))/length(y)
summary(m3)
#0.9872107

#################################
###model2(full model team1-team2)
test.data2 = arr.data[,c(14:25)]
d = seq(10,54720,10)
d1 = test.data2[d-9,]
d2 = test.data2[d-8,]
d3 = test.data2[d-7,]
d4 = test.data2[d-6,]
d5 = test.data2[d-5,]
d6 = test.data2[d-4,]
d7 = test.data2[d-3,]
d8 = test.data2[d-2,]
d9 = test.data2[d-1,]
d10 = test.data2[d,]
r2.data = cbind(new.data,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)
write.csv(r2.data,"r2data.csv")
r2.data = read.csv("r2data.csv",head=TRUE)
r2.data = as_tibble(r2.data)
r2.data[,14:25] = r2.data[,14:25]-r2.data[,26:37]
r2.data[,38:49] = r2.data[,38:49]-r2.data[,50:61]
r2.data[,62:73] = r2.data[,62:73]-r2.data[,74:85]
r2.data[,86:97] = r2.data[,86:97]-r2.data[,98:109]
r2.data[,110:121] = r2.data[,110:121]-r2.data[,122:133]
r2.data = r2.data[,c(3,14:25,38:49,62:73,86:97,110:121)]
r2.data <- as.matrix(r2.data)
n = dim(r2.data)[1]
kickout = c(7,12,19,24,31,36,43,48,55,60)
#kick out both level and gold
r2.data = r2.data[,-kickout]
train = sample(n,floor(n*0.7))
train_x<-r2.data[train,2:56]
train_x = apply(train_x,2,scale)
train_y<-r2.data[train,1]
test_x<-r2.data[-train,2:56]
test_x = apply(test_x,2,scale)
test_y<-r2.data[-train,1]
#################################
#1
train_y<-as.factor(train_y)
test_y<-as.factor(test_y)
m2.1<-randomForest(x=train_x,y=train_y)
p2.1<-predict(m2.1,test_x,type="response")
CrossTable(test_y,p2.1)
#0.9859927
#2
train_y<-as.factor(train_y)
test_y<-as.factor(test_y)
m2.2<-C5.0(x=train_x,y=train_y,trials = 100)
p2.2<-predict(m2.2,test_x,type="class")
CrossTable(test_y,p2.2)
#0.9847747
#3
train_x<-as_tibble(train_x)
test_x = as_tibble(test_x)
test_y = as.numeric(test_y) -1
m2.3 = glm(train_y~.,data = train_x,family = binomial())
p2.3 = predict(m2.3,test_x,type="response")
p2.3 = (p2.3>=0.5)
r2.3 = 1-sum(abs(test_y-p2.3))/length(test_y)
#0.9841657
summary(m2.3)

#AIC and BIC base on model2.3
model.AIC = step(m2.3, k=2)
p3 = predict(model.AIC,test_x,type="response")
p3 = (p3>=0.5)
r3 = 1-sum(abs(test_y-p3))/length(test_y)
summary(model.AIC)
#0.9866017
model.BIC = step(m2.3,k =log(dim(train_x)[1]))
p4 = predict(model.BIC,test_x,type="response")
p4 = (p4>=0.5)
r4 = 1-sum(abs(test_y-p4))/length(test_y)
summary(model.BIC)
#0.9829476

##BIC is not very stable, so we use aic
