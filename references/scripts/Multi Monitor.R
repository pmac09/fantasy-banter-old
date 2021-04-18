# Load supercoach functions
source('./references/functions/load.R')

bets <- as_tibble(as.data.frame(rbind(
  c('1', 'Game', 'Points (Under)', 114),
  c('1', 'Richmond', 'Margin (Under)', 39),
  c('1', 'Vlastuin', 'Disposals', 15),
  c('1', 'Bolton', 'Disposals', 15),
  c('1', 'Guthrie', 'Disposals', 15),
  c('1', 'Ablett jnr', 'Goals', 1),
  c('1', 'Miers', 'Goals', 1),
  c('-----','----------','--------------', NA),
  c('2', 'Game', 'Points (Under)', 114),
  c('2', 'Geelong', 'Margin (Under)', 39),
  c('2', 'Graham', 'Disposals', 15),
  c('2', 'Duncan', 'Disposals', 20),
  c('2', 'Bolton', 'Goals', 1),
  c('2', 'Rioli', 'Goals', 1),
  c('2', 'Rohan', 'Goals', 1),
  c('-----','----------','--------------', NA),
  c('3', 'Tri', 'Tribet', 24),
  c('3', 'Graham', 'Disposals', 15),
  c('3', 'Ablett jnr', 'Goals', 1),
  c('-----','----------','--------------', NA),
  c('4', 'Game', 'Points (Over)', 83),
  c('4', 'Richmond', 'Line (+)', 30),
  c('4', 'Geelong', 'Win Most Qtrs', 2)
))) 

names(bets) <- c('BET','WHO','WHAT','MARGIN')

bets <- bets %>%
  mutate(MARGIN = as.numeric(MARGIN))

bets


ff_fixture <- ff_get_fixture_data(live = TRUE)
ff_game_id <- ff_fixture$game_id[1]


ff_game <- suppressWarnings(ff_get_game_data(ff_game_id))

stats <- ff_game[,c('player.last_name', 'kicks', 'handballs', 'goals')] %>%
  mutate(disposals = as.numeric(kicks) + as.numeric(handballs)) %>%
  mutate(goals = as.numeric(goals)) %>%
  rename(WHO = player.last_name) %>%
  select(WHO, disposals, goals)

team_stats <- ff_game[,c('gametime','player.team.abbrev', 'goals', 'behinds')] %>%
  rename(team = player.team.abbrev) %>%
  mutate(score = 6*as.numeric(goals) + as.numeric(behinds)) %>%
  group_by(gametime,team) %>%
  summarise(score = sum(score),
            .groups='drop')

team_stats2 <- left_join(team_stats,team_stats, by=c('gametime')) %>%
  filter(team.x != team.y) %>%
  mutate(Margin = score.x - score.y) %>%
  rename(WHO = team.x) %>%
  mutate(WHO = ifelse(WHO == 'GE', 'Geelong','Richmond')) %>%
  select(WHO, Margin) %>%
  add_row(WHO='Tri', Margin=max(team_stats$score)- min(team_stats$score))
                         
                  
game_stats <- team_stats %>%
  summarise(Points = sum(score)) %>%
  add_column(WHO = 'Game') %>%
  select(WHO, Points)


fnl <- bind_rows(game_stats, team_stats2)
fnl <- bind_rows(fnl, stats)

message(Sys.time())

result <- left_join(bets, fnl, by=c('WHO')) %>%
  mutate(RESULT = case_when(
    WHAT == 'Points (Under)' ~ ifelse(Points < MARGIN, 'WIN', as.character(Points)),
    WHAT == 'Margin (Under)' ~ ifelse(Margin > 0 & Margin <= MARGIN, 'WIN', as.character(Margin)),
    WHAT == 'Disposals'      ~ ifelse(disposals >= MARGIN, 'WIN', as.character(disposals)),
    WHAT == 'Goals'          ~ ifelse(goals >= MARGIN, 'WIN', as.character(goals)),
    WHAT == 'Points (Over)'  ~ ifelse(Points > MARGIN, 'WIN', as.character(Points)),
    WHAT == 'Line (+)'       ~ ifelse(Margin + MARGIN >0, 'WIN', as.character(Margin)),
    WHAT == 'Tribet'         ~ ifelse(Margin <= MARGIN, 'WIN', as.character(Margin))
  )) %>%
  select(BET, WHO, WHAT, MARGIN, RESULT)
  
print(result, n=100)


