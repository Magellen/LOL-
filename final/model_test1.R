###test basic model

source("data transformation.R")
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

##
table = table3
dt = new.data %>% 
  mutate(K = table$kill[ID1]+table$kill[ID2]+table$kill[ID3]+table$kill[ID4]+table$kill[ID5]-
           (table$kill[ID6]+table$kill[ID7]+table$kill[ID8]+table$kill[ID9]+table$kill[ID10]),
         D = table$death[ID1]+table$death[ID2]+table$death[ID3]+table$death[ID4]+table$death[ID5]-
           (table$death[ID6]+table$death[ID7]+table$death[ID8]+table$death[ID9]+table$death[ID10]),
         A = table$assit[ID1]+table$assit[ID2]+table$assit[ID3]+table$assit[ID4]+table$assit[ID5]-
           (table$assit[ID6]+table$assit[ID7]+table$assit[ID8]+table$assit[ID9]+table$assit[ID10])
         )

test= glm(Winner~(K+D+A)^3,data=dt,family = binomial())
summary(test)
plot(test$fitted.values)









