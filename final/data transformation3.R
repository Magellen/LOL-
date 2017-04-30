###transform the raw data


library(tidyverse)
raw.data = read.csv("data000.csv")
#use the correct file name
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
arr.data = temp.data %>% 
  arrange(matchID, lane, role)
new.data = arr.data %>% 
  select(matchID, championID, Winner, teamID) %>% 
  unite(win, teamID, Winner, sep="") %>%
  mutate(Winner = ((win=="100 True")|(win=="200 False")), type = rep(ID, m)) %>%
  select(matchID, championID, Winner, type) %>% 
  spread(key = type, value = championID) %>% 
  mutate(matchID = 1:m)

