###transform the raw data

library(tidyverse)
raw.data = read.csv("game_data.csv",header = T)
raw.data = as_tibble(raw.data)
raw.data$gameLength = raw.data$gameLength/60
#note:change the unit from second into minute
raw.data$totalTimeCrowdControlDealt = raw.data$totalTimeCrowdControlDealt/5
colnames(raw.data)[26] = "averageTimeCrowdControlDealt"
#note:change the variable from total to average, which makes more sense

##type 1 normal
data1 = raw.data %>% 
  group_by(championID) %>% 
  summarise(kill = mean(K), death = mean(D), assit = mean(A),
            dam = mean(dam), dToC = mean(dToC), lv = mean(lvl), 
            damToken = mean(damToken), heal = mean(heal), minionKill = mean(minionKill),
            neutralMinionsKilled = mean(neutralMinionsKilled), gold = mean(goldEarned),
            averageTimeCrowdControlDealt = mean(averageTimeCrowdControlDealt),
            gameLength = mean(gameLength)
            )
    
##type 2 divide by time
k = c(18,19,21:26)
#note:variables to divide by time
#choose: dam	dToC damToken	heal	minionKill	
#neutralMinionsKilled	goldEarned	averageTimeCrowdControlDealt
temp.data = raw.data
temp.data[,k] = raw.data[,k]/unlist(raw.data[,27])
data2 = temp.data %>% 
  group_by(championID) %>% 
  summarise(kill = mean(K), death = mean(D), assit = mean(A),
            dam = mean(dam), dToC = mean(dToC), lv = mean(lvl), 
            damToken = mean(damToken), heal = mean(heal), minionKill = mean(minionKill),
            neutralMinionsKilled = mean(neutralMinionsKilled), gold = mean(goldEarned),
            averageTimeCrowdControlDealt = mean(averageTimeCrowdControlDealt)
            )

##type 3 center and scale according to position based on type 2
temp.data2 = temp.data %>% 
  group_by(lane,role) %>% 
  mutate(K = scale(K), D = scale(D), A = scale(A),
         dam = scale(dam), dToC = scale(dToC), lvl = scale(lvl), 
         damToken = scale(damToken), heal = scale(heal), minionKill = scale(minionKill),
         neutralMinionsKilled = scale(neutralMinionsKilled), gold = scale(goldEarned),
         averageTimeCrowdControlDealt = scale(averageTimeCrowdControlDealt)) 
data3 = temp.data2 %>% 
  group_by(championID) %>% 
  summarise(kill = mean(K), death = mean(D), assit = mean(A),
            dam = mean(dam), dToC = mean(dToC), lv = mean(lvl), 
            damToken = mean(damToken), heal = mean(heal), minionKill = mean(minionKill),
            neutralMinionsKilled = mean(neutralMinionsKilled), gold = mean(goldEarned),
            averageTimeCrowdControlDealt = mean(averageTimeCrowdControlDealt)
            )

  
  
  
  



