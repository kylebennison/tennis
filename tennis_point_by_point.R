library(data.table)
library(tidyverse)
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")

# Data: https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-matches.csv
raw <- fread("https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-points.csv")



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

# Analysis ----------------------------------------------------------------

# NOTE:
# Cannot group by rallyCount and server alone, because inherently, if you are serving,
# every "1 shot" rally you will win, because that's an Ace, every 2 shot rally you lose, and so on...
# So need to bucket them

# Rally Count of 0 is a double fault

### Serve point win %

# Point win percentage grouped by player with x-axis of rally-length

rally_wins <- tennis_clean %>% 
  filter(rallyCount > 0) %>% 
  mutate(rally_bucket = round(rallyCount/2 + .01, digits = 0)) %>% 
  group_by(server, rally_bucket) %>% 
  summarise(wins = sum(isSvrWinner),
            n_points = n()) %>% 
  mutate(win_rate = wins / n_points) %>% 
  select(server, rally_bucket, wins, n_points, win_rate) %>% 
  arrange(desc(server), desc(rally_bucket)) %>% 
  mutate(bucket_name = paste0(rally_bucket*2-1,"-", rally_bucket*2))

rally_wins %>% 
  filter(n_points >= 10) %>% 
  filter(server %in% c("Novak Djokovic", "Rafael Nadal", "Roger Federer", "Daniil Medvedev", "Stefanos Tsitsipas",
                       "Dominic Thiem", "Alexander Zverev", "Andrey Rublev", "Matteo Berrettini",
                       "Denis Shapovalov", "Roberto Bautista Agut", "Diego Schwartzman")) %>% 
  ggplot(aes(x = rally_bucket, y = win_rate)) +
  geom_smooth() +
  facet_wrap(vars(server))

# "" x-axis of point # in the game (longer games with more deuces)
point_wins <- tennis_clean %>% 
  group_by(server, game_point) %>% 
  summarise(wins = sum(isSvrWinner),
            n_points = n()) %>% 
  mutate(win_rate = wins / n_points) %>% 
  select(server, game_point, wins, n_points, win_rate) %>% 
  arrange(desc(server), desc(game_point))

point_wins %>% 
  filter(n_points >= 10) %>% 
  filter(server %in% c("Novak Djokovic", "Rafael Nadal", "Roger Federer", "Daniil Medvedev", "Stefanos Tsitsipas",
                       "Dominic Thiem", "Alexander Zverev", "Andrey Rublev", "Matteo Berrettini",
                       "Denis Shapovalov", "Roberto Bautista Agut", "Diego Schwartzman")) %>% 
  ggplot(aes(x = game_point, y = win_rate)) +
  geom_smooth() +
  facet_wrap(vars(server))

point_wins %>% 
  filter(n_points >= 30) %>% 
  ggplot(aes(x = as.factor(game_point), y = win_rate)) +
  geom_boxplot()

# "" x-axis sets in the match (longer matches with more points played)
set_wins <- tennis_clean %>% 
  group_by(server, set_num) %>% 
  summarise(wins = sum(isSvrWinner),
            n_matches = n()) %>% 
  mutate(win_rate = wins / n_matches) %>% 
  select(server, set_num, wins, n_matches, win_rate) %>% 
  arrange(desc(server), desc(set_num))

set_wins %>% 
  filter(n_matches >= 10) %>% 
  filter(server %in% c("Novak Djokovic", "Rafael Nadal", "Roger Federer", "Daniil Medvedev", "Stefanos Tsitsipas",
                       "Dominic Thiem", "Alexander Zverev", "Andrey Rublev", "Matteo Berrettini",
                       "Denis Shapovalov", "Roberto Bautista Agut", "Diego Schwartzman")) %>% 
  ggplot(aes(x = set_num, y = win_rate)) +
  geom_col() +
  facet_wrap(vars(server))

# "" x-axis total points played in the match (longer matches with more points played)
match_wins <- tennis_clean %>% 
  mutate(pt_bucket = (trunc(Pt/100) + 1) * 100) %>% 
  group_by(server, pt_bucket) %>% 
  summarise(wins = sum(isSvrWinner),
            n_points = n()) %>% 
  mutate(win_rate = wins / n_points) %>% 
  select(server, pt_bucket, wins, n_points, win_rate) %>% 
  arrange(desc(server), desc(pt_bucket))

match_wins %>% 
  filter(n_points >= 30) %>% 
  filter(server %in% c("Novak Djokovic", "Rafael Nadal", "Roger Federer", "Daniil Medvedev", "Stefanos Tsitsipas",
                       "Dominic Thiem", "Alexander Zverev", "Andrey Rublev", "Matteo Berrettini",
                       "Denis Shapovalov", "Roberto Bautista Agut", "Diego Schwartzman")) %>% 
  ggplot(aes(x = pt_bucket, y = win_rate)) +
  geom_line() +
  facet_wrap(vars(server))


# Play Compared to Opponent -----------------------------------------------

tennis_clean2 <- tennis_clean %>% 
  mutate(first_player_won = case_when(first_player_initials == Serving & isSvrWinner == 1 ~ 1,
                                      first_player_initials != Serving & isSvrWinner == 1 ~ 0,
                                      first_player_initials == Serving & isSvrWinner == 0 ~ 0,
                                      first_player_initials != Serving & isSvrWinner == 0 ~ 1,
                                      TRUE ~ 0),
         second_player_won = 1 - first_player_won) %>% 
  mutate(rally_bucket = round(rallyCount/2 + .01, digits = 0)) %>% 
  mutate(pt_bucket = (trunc(Pt/100) + 1) * 100)

# Point win rate by set compared to opponent

tennis_clean_set <- tennis_clean2 %>% 
  group_by(first_player, set_num) %>% 
  summarise(point_wins = sum(first_player_won),
            opponent_wins = sum(second_player_won),
            n_points = n()) %>% 
  mutate(win_rate = point_wins / n_points,
         opponent_win_rate = opponent_wins / n_points)

tennis_clean_set %>% 
  pivot_longer(c(win_rate, opponent_win_rate), names_to = "stat", values_to = "value") %>% 
  filter(n_points >= 50) %>% 
  filter(first_player %in% c("Novak Djokovic", "Rafael Nadal", "Roger Federer", "Daniil Medvedev", "Stefanos Tsitsipas",
                       "Dominic Thiem", "Alexander Zverev", "Andrey Rublev", "Matteo Berrettini",
                       "Denis Shapovalov", "Roberto Bautista Agut", "Diego Schwartzman")) %>% 
  ggplot(aes(x = set_num, y = value)) +
  geom_line(aes(color = stat), size = 2) +
  facet_wrap(vars(first_player)) +
  staturdays_theme +
  labs(title = "Share of Points Won by Set",
       subtitle = "Min. 50 points played - Since March 2019",
       x = "Set",
       y = "% Points Won",
       caption = "@kylebeni012 for @staturdays | Data courtesy @tennisabstract") +
  scale_color_manual(labels = c("Opponent Win Rate", "Win Rate"), 
                     values = c("#acabb8", 
                                as.character(staturdays_colors("dark_blue")))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.title = element_blank(),
        legend.position = c(.75, .15))

ggsave(filename = "C:/Users/Kyle/Documents/Kyle/Personal/Data/Tennis/share_of_points_by_set.png",
       plot = last_plot(),
       width = 400,
       height = 200,
       units = "mm")
  

# Point win rate by rally_bucket

tennis_clean_rally <- tennis_clean2 %>% 
  group_by(first_player, rally_bucket) %>% 
  summarise(point_wins = sum(first_player_won),
            opponent_wins = sum(second_player_won),
            n_points = n()) %>% 
  mutate(win_rate = point_wins / n_points,
         opponent_win_rate = opponent_wins / n_points) %>% 
  filter(rally_bucket > 0) %>% 
  mutate(bucket_name = paste0(rally_bucket*2-1,"-", rally_bucket*2))

tennis_clean_rally %>% 
  pivot_longer(c(win_rate, opponent_win_rate), names_to = "stat", values_to = "value") %>% 
  filter(n_points >= 50) %>% 
  filter(first_player %in% c("Novak Djokovic", "Rafael Nadal", "Roger Federer", "Daniil Medvedev", "Stefanos Tsitsipas",
                             "Dominic Thiem", "Alexander Zverev", "Andrey Rublev", "Matteo Berrettini",
                             "Denis Shapovalov", "Roberto Bautista Agut", "Diego Schwartzman")) %>% 
  ggplot(aes(x = rally_bucket, y = value)) +
  geom_line(aes(color = stat), size = 2) +
  facet_wrap(vars(first_player),
             nrow = 5) +
  staturdays_theme +
  labs(title = "Share of Points Won by Rally Length",
       subtitle = "Min. 50 points played - Since March 2019",
       x = "Rally Length",
       y = "% Points Won",
       caption = "@kylebeni012 for @staturdays | Data courtesy @tennisabstract") +
  scale_color_manual(labels = c("Opponent Win Rate", "Win Rate"), 
                     values = c("#acabb8", 
                                as.character(staturdays_colors("dark_blue")))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.title = element_blank(),
        legend.position = c(.1, -.1),
        legend.direction = "horizontal") + 
  scale_x_continuous(breaks = seq(1,9, 1), 
                     labels = c("1-2", "3-4", "5-6", "7-8", "9-10", 
                                "11-12", "13-14", "15-16", "17-18"))

ggsave(filename = "C:/Users/Kyle/Documents/Kyle/Personal/Data/Tennis/share_of_points_by_rally.png",
       plot = last_plot(),
       width = 400,
       height = 200,
       units = "mm")

# Point win rate by game_point 

tennis_clean_game_point <- tennis_clean2 %>% 
  group_by(first_player, game_point) %>% 
  summarise(point_wins = sum(first_player_won),
            opponent_wins = sum(second_player_won),
            n_points = n()) %>% 
  mutate(win_rate = point_wins / n_points,
         opponent_win_rate = opponent_wins / n_points)

tennis_clean_game_point %>% 
  pivot_longer(c(win_rate, opponent_win_rate), names_to = "stat", values_to = "value") %>% 
  filter(n_points >= 50) %>% 
  filter(first_player %in% c("Novak Djokovic", "Rafael Nadal", "Roger Federer", "Daniil Medvedev", "Stefanos Tsitsipas",
                             "Dominic Thiem", "Alexander Zverev", "Andrey Rublev", "Matteo Berrettini",
                             "Denis Shapovalov", "Roberto Bautista Agut", "Diego Schwartzman")) %>% 
  ggplot(aes(x = game_point, y = value)) +
  geom_line(aes(color = stat), size = 2) +
  facet_wrap(vars(first_player)) +
  staturdays_theme +
  labs(title = "Share of Points Won by Point in Game",
       subtitle = "Min. 50 points played - Since March 2019",
       x = "Point # in Game",
       y = "% Points Won",
       caption = "@kylebeni012 for @staturdays | Data courtesy @tennisabstract") +
  scale_color_manual(labels = c("Opponent Win Rate", "Win Rate"), 
                    values = c("#acabb8", 
                               as.character(staturdays_colors("dark_blue")))) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.title = element_blank(),
        legend.position = c(.1, -.1),
        legend.direction = "horizontal")

ggsave(filename = "C:/Users/Kyle/Documents/Kyle/Personal/Data/Tennis/share_of_points_game.png",
       plot = last_plot(),
       width = 400,
       height = 200,
       units = "mm")

# Point win rate by match point bucket

tennis_clean_match_point <- tennis_clean2 %>% 
  group_by(first_player, pt_bucket) %>% 
  summarise(point_wins = sum(first_player_won),
            opponent_wins = sum(second_player_won),
            n_points = n()) %>% 
  mutate(win_rate = point_wins / n_points,
         opponent_win_rate = opponent_wins / n_points)

tennis_clean_match_point %>% 
  pivot_longer(c(win_rate, opponent_win_rate), names_to = "stat", values_to = "value") %>% 
  filter(n_points >= 50) %>% 
  filter(first_player %in% c("Novak Djokovic", "Rafael Nadal", "Roger Federer", "Daniil Medvedev", "Stefanos Tsitsipas",
                             "Dominic Thiem", "Alexander Zverev", "Andrey Rublev", "Matteo Berrettini",
                             "Denis Shapovalov", "Roberto Bautista Agut", "Diego Schwartzman")) %>% 
  ggplot(aes(x = pt_bucket, y = value)) +
  geom_line(aes(color = stat), size = 2) +
  facet_wrap(vars(first_player)) +
  staturdays_theme +
  labs(title = "Share of Points Won by Total Points Played",
       subtitle = "Min. 50 points played in bucket - Since March 2019",
       x = "Points played (at minimum)",
       y = "% Points Won",
       caption = "@kylebeni012 for @staturdays | Data courtesy @tennisabstract") +
  scale_color_manual(labels = c("Opponent Win Rate", "Win Rate"), 
                     values = c("#acabb8", 
                                as.character(staturdays_colors("dark_blue")))) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.title = element_blank(),
        legend.position = c(.75, .15))

ggsave(filename = "C:/Users/Kyle/Documents/Kyle/Personal/Data/Tennis/share_of_points_match.png",
       plot = last_plot(),
       width = 400,
       height = 200,
       units = "mm")



#### Serving map
fed <- tennis_clean %>% filter(server == "Roger Federer") %>% 
  mutate(serve_direction = substring(`1st`,1,1))

fed_aggregated <- fed %>% group_by(serve_direction) %>% 
  count() %>% 
  filter(serve_direction %in% c("4", "5", "6"))

fed_aggregated <- fed_aggregated %>% 
  mutate(perc = round(100*(n/sum(fed_aggregated$n)),1)) %>% 
  arrange(serve_direction)

ggplot() + geom_hline(yintercept=0)+
  geom_segment(aes(x = -13.5, y = 21, xend = 13.5, yend = 21)) +
  geom_segment(aes(x = -0, y = 0, xend = 0, yend =21)) +
  geom_segment(aes(x = -18, y = 0, xend = 18, yend = 0)) +
  geom_segment(aes(x = -13.5, y = 0, xend = -13.5, yend = 39)) +
  geom_segment(aes(x = 13.5, y = 0, xend = 13.5, yend = 39)) +
  geom_segment(aes(x = -18, y = 0, xend = -18, yend = 39)) +
  geom_segment(aes(x = 18, y = 0, xend = 18, yend = 39)) +
  #service boxes
  geom_segment(aes(x = -18, y = 39, xend = 18, yend = 39)) +
  #dfdf
  annotate("rect", xmin = -13.5, xmax = -9, ymin = 0, ymax = 21,
           alpha = fed_aggregated[1,3]/100) +
  annotate("rect", xmin = -9, xmax = -4.5, ymin = 0, ymax = 21,
           alpha = fed_aggregated[2,3]/100) +
  annotate("rect", xmin = -4.5, xmax = 0, ymin = 0, ymax = 21,
           alpha = fed_aggregated[3,3]/100) +
  annotate("text", x =-11.25, y = 18, label = fed_aggregated[1,3]) +
  annotate("text", x =-6.75, y = 12, label = fed_aggregated[2,3]) +
  annotate("text", x =-2.25, y = 6, label = fed_aggregated[3,3]) +
  labs(x="", y="") +
  theme_void()
  

