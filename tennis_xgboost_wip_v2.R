library(data.table)
library(tidyverse)
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")

# Data: https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-matches.csv
raw <- fread("https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-points.csv")


# Clean and mutate data ---------------------------------------------------

tennis_small <- raw %>% 
  mutate(player_names = str_extract(match_id, "[^-]+-[^-]+$")) # Get the text to the right of the second-to-last "-"

rm(raw)

tennis_test <- tennis_small %>% 
  mutate(first_player = str_extract(player_names, "^[^-]+")) %>% 
  mutate(second_player = str_extract(player_names, "[^-]+$")) %>% 
  mutate(first_player_initials = str_replace_all(first_player, "[^A-Z]", ""),
         second_player_initials = str_replace_all(second_player, "[^A-Z]", ""))

tennis_clean <- tennis_test %>% 
  mutate(server = case_when(Serving == first_player_initials ~ first_player,
                            TRUE ~ second_player)) %>% 
  mutate(server = str_replace(server, "_", " "),
         first_player = str_replace(first_player, "_", " "),
         second_player = str_replace(second_player, "_", " ")) %>% 
  mutate(game_point = str_extract(`Gm#`, "\\([^()]+\\)"),
         game_point = str_replace_all(game_point, "[\\(\\)]", ""),
         game_point = as.integer(game_point)) %>% 
  mutate(set_num = Set1 + Set2 + 1)

tennis_clean2 <- tennis_clean %>%
  mutate(
    first_player_won = case_when(
      first_player_initials == Serving & isSvrWinner == 1 ~ 1,
      first_player_initials != Serving &
        isSvrWinner == 1 ~ 0,
      first_player_initials == Serving &
        isSvrWinner == 0 ~ 0,
      first_player_initials != Serving &
        isSvrWinner == 0 ~ 1,
      TRUE ~ 0
    ),
    second_player_won = 1 - first_player_won
  )

tennis_clean3 <- tennis_clean2 %>%
  mutate(
    sets_needed_to_win = case_when(str_detect(
      match_id,
      "US_Open") ~ 3L,
      str_detect(match_id, "Wimbledon") ~ 3L,
      str_detect(match_id, "Roland_Garros") ~ 3L,
      str_detect(match_id, "Australian_Open") ~ 3L,
      TRUE ~ 2L),
    p1_game_points_pre_serve = if_else(
      Svr == 1,
      str_extract(Pts, "^[^-]+"), # Get everything from the start of the string that's not a hyphen
      str_extract(Pts, "[^-]+$")  # Same but start from the end of the string
    ),
    p2_game_points_pre_serve = if_else(
      Svr == 2,
      str_extract(Pts, "^[^-]+"),
      str_extract(Pts, "[^-]+$")
    ),
    p1_game_points_pre_serve = ifelse(p1_game_points_pre_serve == "AD", 45L, p1_game_points_pre_serve), # Making Advantage = a score of 45 so it works as an integer with xgboost later
    p2_game_points_pre_serve = ifelse(p2_game_points_pre_serve == "AD", 45L, p2_game_points_pre_serve) # Be honest you thought advantage was basically 45 points already
  )

match_winners <- tennis_clean3 %>% 
  group_by(match_id) %>% 
  filter(Pt == max(Pt)) %>% 
  mutate(match_winner = case_when(Gm1 > Gm2 ~ first_player,
                                  Gm2 > Gm1 ~ second_player,
                                  PtWinner == 1 ~ first_player,
                                  PtWinner == 2 ~ second_player,
                                  TRUE ~ "winner_unknown")) %>% 
  select(match_id, match_winner)

data_cleaned <- tennis_clean3 %>% 
  left_join(match_winners)

rm(list = c("tennis_clean", "tennis_clean2", "tennis_clean3", "tennis_small", "tennis_test", "match_winners"))

# Start of building win probability model ---------------------------------

# Add match outcome for first player
data_cleaned <- data_cleaned %>% 
  mutate(player_1_outcome = as.integer(if_else(first_player == match_winner, 1, 0)),
         p1_game_points_pre_serve = as.integer(p1_game_points_pre_serve),
         p2_game_points_pre_serve = as.integer(p2_game_points_pre_serve))

data_cleaned <- data_cleaned %>% 
  filter(sets_needed_to_win == 3)

plays.make.end.rows <- data_cleaned %>% 
  group_by(match_id) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  mutate(Set1 = ifelse(player_1_outcome==1, 3, Set1),
         Set2 = ifelse(player_1_outcome==1,Set2,3),
         Pt=Pt+1,
         Gm1 = ifelse(player_1_outcome==1,Gm1+1,Gm1),
         Gm2 = ifelse(player_1_outcome==1,Gm2,Gm2+1))

data_cleaned <- rbind(data_cleaned, plays.make.end.rows)

#remove anything related to points
data_cleaned2 <- data_cleaned %>% 
  group_by(match_id, Set1, Set2, Gm1, Gm2) %>% 
  filter(row_number()==1) %>% 
  mutate(match_over = ifelse(Set1==3 | Set2==3, 1, 0),
         set_num = ifelse(match_over==1,6,set_num)) %>% 
  ungroup()

# Split data
set.seed(1234)

y.train <- data_cleaned2$player_1_outcome

x.train <- data_cleaned2 %>% 
  select(Pt, Set1, Set2, Gm1, Gm2, Svr, set_num,
         sets_needed_to_win, match_over) %>% 
  as.matrix()

x.train.leftover <- data_cleaned2 %>% 
  select(-c(Pt, Set1, Set2, Gm1, Gm2, Svr, set_num,
            sets_needed_to_win, match_over))

x.test <- data_cleaned2 %>%
  filter(match_id == "20210613-M-Roland_Garros-F-Stefanos_Tsitsipas-Novak_Djokovic") %>% 
  select(Pt, Set1, Set2, Gm1, Gm2, Svr, set_num,
         sets_needed_to_win, match_over) %>% 
  as.matrix()

x.test.leftover <- data_cleaned2 %>%
  filter(match_id == "20210613-M-Roland_Garros-F-Stefanos_Tsitsipas-Novak_Djokovic") %>% 
  select(-c(Pt, Set1, Set2, Gm1, Gm2, Svr, set_num,
            sets_needed_to_win, match_over)) %>% 
  as.matrix()

# Prep for XGboost

library(xgboost)

dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)

# Use cross validation 
param <- list(  objective           = "binary:logistic",
                gamma               = 0.04, #.02
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.06,
                max_depth           = 5,
                min_child_weight    = 2,
                subsample           = .9,
                colsample_bytree    = 1,
                tree_method = 'hist'
)

#run this for training, otherwise skip
XGBm <- xgb.cv(params=param,nfold=5,nrounds=500,missing=NA,data=dtrain,print_every_n=10, early_stopping_rounds = 25)

#train the full model
watchlist <- list( train = dtrain)
XGBm <- xgb.train( params=param,nrounds=150,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=100)

library(zoo)

#current x.test - djokobic 1
x.test <- tibble(Pt=140, Set1=1, Set2=1, Gm1=0, Gm2=0, Svr=1, set_num=3, sets_needed_to_win=3,
                 match_over=0) %>% 
  as.matrix()

res <- x.test %>% as_tibble()
dtest <- xgb.DMatrix(x.test,missing=NA)
res$winprob <- predict(XGBm, newdata = dtest)
res$winprob

res %>% 
  cbind(x.test.leftover) %>% 
  ggplot(aes(x=Pt, y=winprob)) +geom_line() + #rollmean(winprob, 5, na.pad = TRUE))
  ylim(0,1) +
  labs(title = "2021 Roland Garros Final - Tsitsipas vs. Djokovic") +
  geom_text(aes(label = paste0(first_player, " Win Probability"),
                 x = 100,
                 y = .95))


#test on full data, and add back in features so we can look at specific plays
x.train.copy <- x.train %>% as_tibble()
x.train.copy$winprob <- predict(XGBm, newdata = dtrain)
x.train.copy$actualhomeresults <- y.train

#add in original extra data
res <- cbind(x.train.copy, x.train.leftover)

#plot any match
match_title <- "20210711-M-Wimbledon-F-Novak_Djokovic-Matteo_Berrettini"

res %>% 
  filter(match_id == match_title) %>% 
  ggplot(aes(x=Pt, y=winprob)) + geom_line() + #rollmean(winprob, 5, na.pad = TRUE))
  ylim(0,1) +
  labs(title = match_title) +
  geom_text(aes(label = paste0(first_player, " Win Probability"),
                x = 100,
                y = .95))


#plot a current match


# Plot predicted vs. actual
res %>% 
  mutate(win_prob_bucket = round(winprob, digits = 2)) %>% 
  group_by(win_prob_bucket) %>% 
  summarise(mean_actual = mean(actualhomeresults)) %>% 
  ggplot(aes(x = win_prob_bucket, y = mean_actual)) +
  geom_point() +
  geom_abline() +
  geom_text(x = .5, y = .2, label = "Overconfident") +
  geom_text(x = .5, y = .9, label = "Underconfident")

# Distribution of probabilities given out
res %>% 
  ggplot(aes(x = winprob)) + 
  geom_density(fill = staturdays_colors("orange"), alpha = .3)

# Variable importance
library(vip)

vip(XGBm)

importance_matric <- xgb.importance(model = XGBm)

xgb.plot.importance(importance_matric, left_margin = 10, cex = .55)

library(pROC)

# This gives a more accurate auc
auc <- pROC::auc(response = res$actualhomeresults, predictor = res$winprob)

ten_roc <- roc(res$actualhomeresults, predict(XGBm, newdata = dtrain))
ten_auc <- toString(ten_roc$auc)

# plot of AUC
ggwin_roc <- ggroc(ten_roc)

ggwin_roc +
  geom_text(mapping = aes(x = 0.5, y = 0.5, label = paste0('AUC of ', round(as.double(ten_auc), 5))))
