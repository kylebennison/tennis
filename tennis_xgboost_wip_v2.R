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
  mutate(first_player_won = case_when(first_player_initials == Serving & isSvrWinner == 1 ~ 1,
                                      first_player_initials != Serving & isSvrWinner == 1 ~ 0,
                                      first_player_initials == Serving & isSvrWinner == 0 ~ 0,
                                      first_player_initials != Serving & isSvrWinner == 0 ~ 1,
                                      TRUE ~ 0),
         second_player_won = 1 - first_player_won) %>% 
  mutate(rally_bucket = round(rallyCount/2 + .01, digits = 0)) %>% 
  mutate(pt_bucket = (trunc(Pt/100) + 1) * 100)

match_winners <- tennis_clean2 %>% 
  group_by(match_id) %>% 
  filter(Pt == max(Pt)) %>% 
  mutate(match_winner = case_when(Gm1 > Gm2 ~ first_player,
                                  Gm2 > Gm1 ~ second_player,
                                  PtWinner == 1 ~ first_player,
                                  PtWinner == 2 ~ second_player,
                                  TRUE ~ "winner_unknown")) %>% 
  select(match_id, match_winner)

data_cleaned <- tennis_clean2 %>% 
  left_join(match_winners)

rm(list = c("tennis_clean", "tennis_clean2", "tennis_small", "tennis_test", "match_winners"))

# Start of building win probability model ---------------------------------

library(tidymodels)

###
# Pivoting longer by player so each player gets their own row for each point

data_player <- data_cleaned %>% 
  pivot_longer(cols = c(first_player, second_player),
               names_to = "player_number", values_to = "player_name") %>% 
  mutate(is_winner = as.factor(if_else(player_name == match_winner, 1, 0)))

# Trying a different method instead
# Taking top 100k rows due to xgboost error about size of object
# Treating outcome as a factor

data_player <- data_cleaned %>% 
  mutate(player_1_outcome = as.integer(if_else(first_player == match_winner, 1, 0))) %>% 
  select(Pt, Set1, Set2, Gm1, Gm2, Svr, `1stIn`, `2ndIn`, game_point, set_num, player_1_outcome) %>% 
  slice_head(n = 100000L)

###

# Split data
set.seed(1234)

y.train <- data_player$player_1_outcome
x.train <- data_player %>% select(-player_1_outcome) %>% 
  as.matrix()
x.leftover <- data_cleaned %>% 
  mutate(player_1_outcome = as.integer(if_else(first_player == match_winner, 1, 0))) %>% 
  select(-c(Pt, Set1, Set2, Gm1, Gm2, Svr, `1stIn`, `2ndIn`, game_point, set_num, player_1_outcome)) %>% 
  slice_head(n = 100000L)

x.test <- data_cleaned %>%
  filter(match_id == "20210620-M-Queens_Club-F-Cameron_Norrie-Matteo_Berrettini") %>% 
  mutate(player_1_outcome = as.factor(if_else(first_player == match_winner, 1, 0))) %>% 
  select(Pt, Set1, Set2, Gm1, Gm2, Svr, `1stIn`, `2ndIn`, game_point, set_num) %>% 
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
                max_depth           = 15,
                min_child_weight    = 2,
                subsample           = 1,
                colsample_bytree    = 1,
                tree_method = 'hist'
)

#run this for training, otherwise skip
XGBm <- xgb.cv(params=param,nfold=5,nrounds=50,missing=NA,data=dtrain,print_every_n=10, early_stopping_rounds = 25)

#train the full model
watchlist <- list( train = dtrain)
XGBm <- xgb.train( params=param,nrounds=60,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=100)

library(zoo)

res <- x.test %>% as_tibble()
res$winprob <- predict(XGBm, newdata = dtest)

res %>% ggplot(aes(x=Pt, y=winprob)) +geom_line() + #rollmean(winprob, 5, na.pad = TRUE))
  ylim(0,1)


#test on full data, and add back in features so we can look at specific plays
x.train.copy <- x.train
x.train.copy <- x.train.copy %>% as_tibble()
x.train.copy$winprob <- predict(XGBm, newdata = dtrain)
x.train.copy$actualhomeresults <- y.train
#add in original extra data
res <- cbind(x.train.copy, x.train.leftover)

#plot any game
x <- res %>% filter(year==2019, week==14, home=="Nebraska", away=="Iowa") %>% 
  ggplot(aes(x=-clock_in_seconds, y=winprob)) +geom_line() + #rollmean(winprob, 5, na.pad = TRUE))
  ylim(0,1)

# Plot predicted vs. actual
res %>% 
  mutate(win_prob_bucket = round(winprob, digits = 2)) %>% 
  group_by(win_prob_bucket) %>% 
  summarise(mean_actual = mean(home_outcome)) %>% 
  ggplot(aes(x = win_prob_bucket, y = mean_actual)) +
  geom_point() +
  geom_abline()


###

tennis_split <- initial_split(data_player, strata = player_1_outcome)
tennis_train <- training(tennis_split)
tennis_test <- testing(tennis_split)