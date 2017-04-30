#####
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
r2.name = read.csv("r2name.csv")
colnames(r2.data) = r2.name$x
r2.data <- as.matrix(r2.data)
n = dim(r2.data)[1]
r2.data = r2.data[,-c(7,12,19,24,31,36,43,48,55,60)]
train = sample(n,floor(n*0.7))
train_x<-r2.data[train,2:51]
train_x = apply(train_x,2,scale)
train_y<-r2.data[train,1]
test_x<-r2.data[-train,2:51]
test_x = apply(test_x,2,scale)
test_y<-r2.data[-train,1]
#################################


#1
train_y<-as.factor(train_y)
test_y<-as.factor(test_y)
m1<-randomForest(x=train_x,y=train_y)
p1<-predict(m1,test_x,type="response")
CrossTable(test_y,p1)
#0.9859927
#2
m2<-C5.0(x=train_x,y=train_y,trials = 100)
p2<-predict(m2,test_x,type="class")
CrossTable(test_y,p2)
#0.9847747
#3
train_x<-as_tibble(train_x)
test_x = as_tibble(test_x)
test_y = as.numeric(test_y)
m3.2 = glm(train_y~.,data = train_x,family = binomial())
p3 = predict(m3.2,test_x,type="response")
p3 = (p3>=0.5)
r3 = 1-sum(abs(test_y-p3))/length(test_y)
#0.9841657
summary(m3.2)

model.AIC = step(m3.2, k=2)
p4 = predict(model.AIC,test_x,type="response")
p4 = (p4>=0.5)
r4 = 1-sum(abs(test_y-p4))/length(test_y)
summary(model.AIC)
