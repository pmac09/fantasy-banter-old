options(stringsAsFactors = FALSE)

library(httr)
library(tidyverse)

source('./references/functions/secrets.R')
source('./references/functions/scrape_supercoach.R')


## Get league data

league_raw <- readRDS(paste0('./references/data/raw/2020_01_SC_TEAMS.RDS'))
ladder_raw <- lapply(league_raw$ladder, unlist)
ladder_raw <- bind_rows(lapply(ladder_raw, as.data.frame.list))
ladder_data <- ladder_raw[,c(
  'user_team_id',
  'userTeam.user_id',
  'userTeam.teamname'
)]

fixture_data <- tibble()
for(i in 1:15){
  
  rnd <- i
  rnd_str <- sprintf("%02d", rnd)
  
  league_raw <- readRDS(paste0('./references/data/raw/2020_',rnd_str,'_SC_TEAMS.RDS'))
  
  fixture_raw <- lapply(league_raw$fixtures, unlist)
  fixture_raw <- bind_rows(lapply(fixture_raw, as.data.frame.list))
  
  if('user_team1.stats.proj_points' %in% colnames(fixture_raw)) {
    fixture_raw <- fixture_raw[,c(
      'round',
      'fixture',
      'user_team1.teamname',
      'user_team1.stats.proj_points',
      'user_team2.teamname',
      'user_team2.stats.proj_points'
    )]
    
  } else {
    fixture_raw <- fixture_raw[,c(
      'round',
      'fixture',
      'user_team1.teamname',
      'user_team1.stats.points',
      'user_team2.teamname',
      'user_team2.stats.points'
    )]
  }

  colnames(fixture_raw)[4] <- 'user_team1.stats.points'
  colnames(fixture_raw)[6] <- 'user_team2.stats.points'
  
  fixture_data <- bind_rows(fixture_data, fixture_raw)

}

rev_fixture_data <- fixture_data[c(1,2,5,6,3,4)]
colnames(rev_fixture_data) <- colnames(fixture_data)

fixture_data <- bind_rows(fixture_data, rev_fixture_data)

fixture_data <- fixture_data %>%
  mutate(round = as.numeric(round)) %>%
  mutate(fixture = as.numeric(fixture)) %>%
  mutate(user_team1.stats.points = as.numeric(user_team1.stats.points)) %>%
  mutate(user_team2.stats.points = as.numeric(user_team2.stats.points)) %>%
  mutate(diff = user_team1.stats.points - user_team2.stats.points) %>%
  mutate(win = ifelse(diff>0,1,0)) %>%
  mutate(draw = ifelse(diff==0,1,0)) %>%
  mutate(loss = ifelse(diff<0,1,0)) 

ladder_data <- fixture_data %>%
  group_by(user_team1.teamname) %>%
  summarise(
    W = sum(win),
    D = sum(draw),
    L = sum(loss),
    PF = sum(user_team1.stats.points),
    PA = sum(user_team2.stats.points)
  ) %>%
  mutate(PTS = W*4 + D*2) %>%
  mutate(PCT = round(PF/PA*100,1)) %>%
  arrange(desc(PTS), desc(PCT))

ladder_data <- ladder_data[c(1,7,2,3,4,5,6,8)]
colnames(ladder_data)[1] <- 'TEAM'

ladder_data



