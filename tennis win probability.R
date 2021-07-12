

raw2 <- fread("https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/2021-wimbledon-points.csv")

raw3 <- raw2 %>% 
  #filter(match_id == "2021-wimbledon-1101") %>% 
  mutate(P1WonSet = ifelse(SetWinner==1, 1, 0),
         P2WonSet = ifelse(SetWinner==2,1,0))  %>% 
  select(match_id, ElapsedTime,SetNo,P1GamesWon, P2GamesWon, SetWinner,GameNo, GameWinner, PointWinner,PointServer, P1Score, P2Score,P1PointsWon, P2PointsWon, ServeIndicator,P1WonSet, P2WonSet) 

#raw3[1,16] <- 0
#raw3[1,17] <- 0

raw4 <- raw3 %>% 
  group_by(match_id) %>% 
  mutate(
        P1WonSetNew = cumsum(P1WonSet),
         P2WonSetNew = cumsum(P2WonSet),
        match_over = ifelse(P1WonSetNew == 3 | P2WonSetNew ==3, 1, 0))

raw4$P1Score <- factor(raw4$P1Score)
raw4$P2Score <- factor(raw4$P2Score)

sets_won <- raw3 %>% group_by(match_id) %>% 
  summarise(P1TotalSets = sum(P1WonSet),
                               P2TotalSets = sum(P2WonSet)) %>% 
  mutate(P1WonMatch = ifelse(P1TotalSets > P2TotalSets, 1, 0)) %>% 
  select(match_id, P1WonMatch)

raw5 <- raw4 %>% left_join(sets_won, by="match_id") %>%
  ungroup() %>% 
  select(-P1WonSet, -P2WonSet, -P1PointsWon, -P2PointsWon,-PointServer,-PointWinner, -GameWinner,
         -SetWinner,-ElapsedTime,-match_id) %>% 
  unique()

mylogit <- glm(P1WonMatch ~ SetNo + P1GamesWon + P2GamesWon + GameNo + P1Score + P2Score + ServeIndicator + P1WonSetNew + P2WonSetNew + match_over, data = raw5, family = "binomial")

today_match <- raw4 %>% filter(match_id == "2021-wimbledon-1601") %>% 
  left_join(sets_won, by="match_id") %>%
  ungroup() %>% 
  select(-P1WonSet, -P2WonSet, -P1PointsWon, -P2PointsWon,-PointServer,-PointWinner, -GameWinner,
         -SetWinner,-ElapsedTime,-match_id) %>% 
  unique()

today_match$winprob1 <- predict(mylogit, newdata = today_match, type = "response")
today_match <- tibble::rowid_to_column(today_match, "ID") 

today_match %>% 
  ggplot(aes(x=ID, y=winprob1)) + geom_line()

#totalsets 1 = previous value + set winner flag