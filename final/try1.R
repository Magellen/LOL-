#try
t<-cbind(new.data,table[new.data$ID1,2:13],table[new.data$ID2,2:13],table[new.data$ID3,2:13],
         table[new.data$ID4,2:13],table[new.data$ID5,2:13],table[new.data$ID6,2:13],
         table[new.data$ID7,2:13],table[new.data$ID8,2:13],table[new.data$ID9,2:13],table[new.data$ID10,2:13])
t1<-t[,c(2,13:132)]
t2<-cbind(t1$Winner,scale(t1[,2:121]))
write.csv(t2,"try2.csv")
t2<-read_csv("try2.csv",head=TRUE)
t2<-as.matrix(t2)
train_x<-t2[1:7000,2:121]
train_y<-t2[1:7000,1]
test_x<-t2[7001:11000,2:121]
test_y<-t2[7001:11000,1]


#1
m<-randomForest(x=train_x,y=train_y)
p<-predict(m,test_x,type="response")
p1<-p>0.5
CrossTable(test_y,p1)
#51.9

#2
train_y<-as.factor(train_y)
test_y<-as.factor(test_y)
m<-C5.0(x=train_x,y=train_y,trials = 100)
p<-predict(m,test_x,type="class")
CrossTable(test_y,p)
#51.4