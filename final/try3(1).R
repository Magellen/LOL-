#try2
temp.data$Winner<-as.character(temp.data$Winner)
temp.data$lane<-as.character(temp.data$lane)
temp.data$role<-as.character(temp.data$role)
wt<-temp.data%>%filter(teamID==100&lane==" TOP")
wm<-temp.data%>%filter(teamID==100&lane==" MIDDLE")
wj<-temp.data%>%filter(teamID==100&lane==" JUNGLE")
wb<-temp.data%>%filter(teamID==100&lane==" BOTTOM")
t<-temp.data%>%filter(teamID=200&lane==" TOP")
m<-temp.data%>%filter(teamID=200&lane==" MIDDLE")
j<-temp.data%>%filter(teamID=200&lane==" JUNGLE")
b<-temp.data%>%filter(teamID=200&lane==" BOTTOM")
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
wt<-temp.data1%>%filter(teamID==100&lane==" TOP")
wm<-temp.data1%>%filter(teamID==100&lane==" MIDDLE")
wj<-temp.data1%>%filter(teamID==100&lane==" JUNGLE")
wb<-temp.data1%>%filter(teamID==100&lane==" BOTTOM")
t<-temp.data1%>%filter(teamID==200&lane==" TOP")
m<-temp.data1%>%filter(teamID==200&lane==" MIDDLE")
j<-temp.data1%>%filter(teamID==200&lane==" JUNGLE")
b<-temp.data1%>%filter(teamID==200&lane==" BOTTOM")
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
odd<-seq(1,64842,2)
even<-seq(2,64842,2)
B<-(wb1[odd,]+wb1[even,]-b1[odd,]-b1[even,])[,2:13]
all<-temp.data1%>%select(matchID,teamID,championID)%>%left_join(table)
all1<-all%>%group_by(matchID,teamID)%>%summarise(dToC=sum(dToC),heal=sum(heal),gold=sum(gold),cc=sum(averageTimeCrowdControlDealt))
all2<-all1%>%filter(teamID==100)
all3<-all1%>%filter(teamID==200)
all2<-as.data.frame(all2[,3:6])
all3<-as.data.frame(all3[,3:6])
All<-all2-all3
#5 features
V<-cbind(T,M,J,B,All)
V1<-data.frame(win=wt$Winner,V)
write_csv(V1,"feature.csv")



