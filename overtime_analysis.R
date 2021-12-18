library(tidyverse)
library(lubridate)
library(ggthemes)



# Import data
# Thanks https://www.hockey-reference.com/ for the data.
# Change your working directory

all_game_results <- read_csv('all_nhl_game_results.csv') 
all_game_results_05_15 <- read_csv('game_results_05-15.csv')

all_game_results <- all_game_results %>% as_tibble()
all_game_results_05_15 <- all_game_results_05_15 %>% as_tibble()

# Rename some columns
all_game_results <- all_game_results %>% 
  rename('Goals_Away' = 'G...3',
         'Goals_Home' = 'G...5' ,
         'Status' = '...6')

all_game_results_05_15 <- all_game_results_05_15 %>% 
  rename('Goals_Away' = 'G...3',
         'Goals_Home' = 'G...5',
         'Status' = '...6')

all_game_results$Date <- ymd(all_game_results$Date)

all_game_results_05_15$Date <- ymd(all_game_results_05_15$Date)



all_game_results <-  all_game_results %>% 
  mutate(Season = 
           case_when(
             Date %within% interval(start = ymd('2021-10-12'), end = ymd('2021-12-15')) ~ '2021-22',
             Date %within% interval(ymd('2021-01-13'), ymd('2021-05-19')) ~ '2020-21',
             Date %within% interval(ymd('2019-10-02'), ymd('2020-03-11')) ~ '2019-20',
             Date %within% interval(ymd('2018-10-03'), ymd('2019-04-06')) ~ '2018-19',
             Date %within% interval(ymd('2017-10-04'), ymd('2018-04-08')) ~ '2017-18',
             Date %within% interval(ymd('2016-10-12'), ymd('2017-04-09')) ~ '2016-17',
             Date %within% interval(ymd('2015-10-07'), ymd('2016-04-10')) ~ '2015-16'
             )
          )

all_game_results_05_15 <- 
all_game_results_05_15 %>% 
  mutate(Season = 
           case_when(
             Date %within% interval(ymd('2005-10-05'), ymd('2006-04-18')) ~ '2005-06',
             Date %within% interval(ymd('2006-10-04'), ymd('2007-04-08')) ~ '2006-07',
             Date %within% interval(ymd('2007-09-29'), ymd('2008-04-06')) ~ '2007-08',
             Date %within% interval(ymd('2008-10-04'), ymd('2009-04-12')) ~ '2008-09',
             Date %within% interval(ymd('2009-10-01'), ymd('2010-04-11')) ~ '2009-10',
             Date %within% interval(ymd('2010-10-07'), ymd('2011-04-10')) ~ '2010-11',
             Date %within% interval(ymd('2011-10-06'), ymd('2012-04-07')) ~ '2011-12',
             Date %within% interval(ymd('2013-01-19'), ymd('2013-04-28')) ~ '2012-13',
             Date %within% interval(ymd('2013-10-01'), ymd('2014-04-13')) ~ '2013-14',
             Date %within% interval(ymd('2014-10-08'), ymd('2015-04-11')) ~ '2014-15'
             )
         )

# Total number of OT / SO since star of 2015 season.
table(all_game_results$Status)

table(all_game_results_05_15$Status)


game_ends <- 
all_game_results %>% 
  group_by(Season, Status) %>% 
  summarize(Count = n()) %>% 
  as_tibble()


game_ends_05_15 <- 
  all_game_results_05_15 %>% 
  group_by(Season, Status) %>% 
  summarize(Count = n()) %>% 
  as_tibble()

game_ends$Status <- 
  game_ends$Status %>% 
  replace_na('Reg')

game_ends_05_15$Status <- 
  game_ends_05_15$Status %>% 
  replace_na('Reg')

knitr::kable(game_ends_05_15)
knitr::kable(game_ends)


games_ends_wider <- pivot_wider(game_ends, names_from = Season, values_from = Count) %>% as_tibble()

game_ends_05_15_wider <- pivot_wider(game_ends_05_15, names_from = Season, values_from = Count) %>% as_tibble()

season_sums <- 
games_ends_wider %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

season_sums <- 
season_sums %>% mutate(Status = 'Total', .before  = '2015-16') 

season_sums <- 
rbind(games_ends_wider, season_sums)

season_sums_longer <- season_sums %>% pivot_longer(!Status,
                                                   names_to = 'Seasons',
                                                   values_to = 'Count')

season_sums_longer <- season_sums_longer %>% arrange(Seasons) %>% relocate(Seasons)


season_sums_05_15 <- 
  game_ends_05_15_wider %>% 
  summarize(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))

season_sums_05_15 <- 
  season_sums_05_15 %>%  mutate(Status = 'Total', .before = '2005-06')

season_sums_05_15 <- 
  rbind(game_ends_05_15_wider, season_sums_05_15)

season_sums_05_15_longer <- season_sums_05_15 %>% pivot_longer(!Status,
                                                           names_to = 'Seasons',
                                                           values_to =  'Count')
season_sums_05_15_longer <- season_sums_05_15_longer %>% arrange(Seasons) %>% relocate(Seasons)


games_played_by_season <- 
  game_ends %>% 
  group_by(Season) %>% 
  summarise(Total = sum(Count))


games_played_by_season_05_15 <- 
  game_ends_05_15 %>% 
  group_by(Season) %>% 
  summarise(Total = sum(Count))


knitr::kable(game_ends_05_15_wider)
knitr::kable(games_ends_wider)

knitr::kable(games_played_by_season_05_15)
knitr::kable(games_played_by_season)





# Start plotting ---------------------------------------------------------------
# Orders least to greatest of importance
status_order <- factor(c('SO', 'OT', 'Reg'))
status_order <- ordered(game_ends$Status, levels = status_order)
game_ends$Status <- status_order

status_order_05_15 <- factor(c('SO', 'OT', 'Reg'))
status_order_05_15 <-ordered(game_ends_05_15$Status, levels = status_order_05_15)
game_ends_05_15$Status <- status_order_05_15



# Bar Charts-------------------------------------------------------------------

# All game ending typed included
# Stacked bar chart


# ggplot(game_ends, aes(fill = Status,
#                       x = Season,
#                       y = Count)) +
#   geom_bar(position = 'dodge',
#            stat = 'identity')
# 
# ggplot(game_ends, aes(fill = Status,
#                       x = Season,
#                       y = Count)) +
#   geom_bar(position = 'stack',
#            stat = 'identity') +
#   labs(title = 'Games by Ending Status per Season',
#        subtitle = '') + 
#   scale_color_economist()



# Grouped bar chart


# ggplot(game_ends, aes(fill = Status,
#                       x = Season,
#                       y = Count)) +
#   geom_bar(position = 'fill',
#            stat = 'identity') + 
#   scale_fill_economist()



game_ends_05_15_percents <- 
  game_ends_05_15 %>% 
  group_by(Season) %>% 
  mutate(pct = prop.table(Count) * 100) %>% 
  ggplot() + aes(Season, pct, fill = Status) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(sprintf('%1.1f', pct), '%')),
            position = position_stack(vjust = 0.5)) +
  scale_fill_economist() +
  theme_bw() +
  labs(
    title = 'Game Endings by Percent',
    subtitle = 'Shoot out added'
  )

game_ends_percents <- 
  game_ends %>% 
  group_by(Season) %>% 
  mutate(pct = prop.table(Count) * 100) %>% 
  ggplot() + aes(Season, pct, fill = Status) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(sprintf('%1.1f', pct), '%')),
            position = position_stack(vjust = 0.5)) +
  scale_fill_economist() + 
  theme_bw() + 
  labs(
    title = 'Game Endings by Percent',
    subtitle = '3v3 Style OT'
  )
  
# Filter out Regulation Games --------------------------------------------------
ot_so_by_season <- 
game_ends %>% filter(Status %in% c('OT', 'SO'))

# Grouped bar chart

# ggplot(ot_so_by_season,  aes(fill = Status, 
#                              x = Season,
#                              y = Count)) +
#   geom_bar(position = 'dodge',
#            stat = 'identity') + 
#   scale_fill_economist()


# Stacked bar chart

ggplot(ot_so_by_season,  aes(fill = Status, 
                             x = Season,
                             y = Count)) +
  geom_bar(position = 'fill',
           stat = 'identity') + 
  scale_fill_economist()


# Line graphs ------------------------------------------------------------------

game_ends %>% 
  ggplot(aes(x = Season,
             y = Count,
             color = Status,
             group = Status
  )) +
  geom_point() +
  geom_line() +
  scale_color_economist() +
  labs(
    titles = 'Number of Game Ending Types Since 2015 Season'
  )
  


ot_so_by_season %>% 
  ggplot(aes(x = Season,
             y = Count,
             color = Status,
             group = Status
             )) +
    geom_point() +
    geom_line() + 
    scale_color_economist() +
    labs(
      title = 'Number of OT/SO Games Since 2015 Season',
      subtitle = '',
      caption = ''
    )
    
season_sums_longer %>% 
  ggplot(aes(x = Seasons,
             y = Count,
             color = Status,
             group = Status
  )) +
  geom_point() +
  geom_line() + 
  scale_color_economist() +
  labs(
    title = 'Number of Games Since 2015 Season',
    subtitle = '',
    caption = ''
  )

season_sums_05_15_longer %>%            
ggplot(aes(x = Seasons,
           y = Count,
           color = Status,
           group = Status
)) +
  geom_point() +
  geom_line() + 
  scale_color_economist() +
  labs(
    title = 'Number of Games Per Season',
    subtitle = '',
    caption = ''
  )





