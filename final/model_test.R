###test basic model

source("data transformation.R")
##transfer raw data for model fitting
ID = c("ID1", "ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10")
m = dim(raw.data)[1]/10
data = raw.data %>% 
  select(matchID, championID, Winner, teamID) %>% 
  unite(win, teamID, Winner, sep="") %>%
  mutate(Winner = ((win=="100 True")|(win=="200 False")), type = rep(ID, m)) %>%
  select(matchID, championID, Winner, type) %>% 
  spread(key = type, value = championID) %>% 
  mutate(matchID = 1:m)

##










