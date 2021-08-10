library(readr)
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)

players <- read.csv("R/EPL_20_21_players.csv", encoding = "UTF-8")
players <- players %>% mutate(Club = as.factor(Club))

# average goals a player has scored by club (among those who have scored)
players %>% group_by(Club) %>%
  filter(Goals > 0) %>% 
  summarize(average_goals_scored_per_player = mean(Goals)) %>% 
  arrange(desc(average_goals_scored_per_player))

# goals per minute in descending order
players %>% filter(Goals > 0 & Mins > 200) %>%
  summarize(Name, goals_per_min = Goals / Mins) %>% 
  arrange(desc(goals_per_min))

# plotting players goals and seeing outliers
no_zero <- players %>% filter(Goals > 0)
ggplot(no_zero, aes(Goals)) + geom_boxplot()

# Expected contributions by forwards and midfielders (min 200 minutes)
players %>% filter(Mins > 200, Position %in% c("MF", "FW")) %>%
  summarize(Name, Club, expected_contributions = xG + xA) %>%
  arrange(desc(expected_contributions))

# Count the number of goals scored by nationality:
nationality <- players %>% group_by(Nationality) %>% filter (Goals > 0 | Assists > 0) %>%
  summarize(Nationality, total_goals = sum(Goals), total_assists = sum(Assists)) %>% 
  arrange(desc(total_goals))
nationality <-unique(nationality) 

pie(nationality$total_goals, labels = nationality$Nationality,
    col = rainbow(10), main = "Goals scored by nationality")

# Teams with best penalty conversation percentage:
penalty_conv <- players %>% group_by(Club) %>% 
  summarize (Club, Penalty_Goals = sum(Penalty_Goals), 
             Penalties_Attempted = sum(Penalty_Attempted), 
             penalty_conv_perc = sum(Penalty_Goals) / sum(Penalty_Attempted)) %>% 
  arrange(desc(penalty_conv_perc))
penalty_conv <- unique(penalty_conv) 

# Which ages had the most goals in the premier league last season?
unique(players %>% group_by(Age) %>% filter(Goals > 0) %>%
  summarize(Age, total_goals = sum(Goals), total_assists = sum(Assists)) %>% 
  arrange(desc(total_goals)))
 
# Which team fielded the most unique players?
players %>% group_by(Club) %>% 
  count(Club) %>% arrange(desc(n))

# Players substituted on the most:
players %>% summarize(Name, Club, substituted_on = Matches - Starts) %>%
  arrange(desc(substituted_on))

# Most goals from defenders last season:
unique(players %>% group_by(Club) %>%
  filter(Goals > 0 & Position %in% "DF") %>% 
  summarize(Club, def_goals = sum(Goals)) %>%
arrange(desc(def_goals)))

#Plot goals by position and separate by club:
goals_by_position <- players %>% group_by(Position, Club) %>%
  summarize(Position, Club, goals = sum(Goals))
goals_by_position <- unique(goals_by_position)
goals_by_position <- goals_by_position %>% filter(!Position %in% c("FW,DF", "DF,FW"))
ggplot(goals_by_position, aes(x=Position, y=goals)) + 
  geom_bar(stat="identity") + facet_wrap (~ Club)

# Which clubs relied on goals from forwards the most:
total_goals <- unique(players %>% group_by(Club) %>%
   summarise(Club, total_goals = sum(Goals)))
forward_goals <- unique(players %>% group_by(Club) %>%
  filter(Goals > 0 & Position %in% "FW") %>% 
  select(Club, forw_goals = sum(players$Goals)))
forward_goals['total_goals'] = total_goals$total_goals
forward_goals['perc_forwards'] = forward_goals$forw_goals / forward_goals$total_goals