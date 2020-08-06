options(stringsAsFactors = FALSE)

library(httr)
library(tidyverse)

source('./references/functions/secrets.R')
source('./references/functions/scrape_supercoach.R')

# Get authentication 
auth_headers <- get_auth(cid, tkn)

# Get game settings
settings <- content(GET(
  url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/settings'),
  config = auth_headers
))

# Get current round
round <- settings$competition$current_round

# Update Supercoach data
for(i in 1:round){
  download_supercoach(i, auth_headers)
}


master_data <- tibble()
for(rnd in 1:round){
  
  rnd_str <- sprintf("%02d", rnd)
  
  # PLAYER DATA  ------
  players_raw <- readRDS(paste0('./references/data/raw/2020_',rnd_str,'_SC_PLAYERS.RDS'))
  players_raw <- lapply(players_raw, unlist)
  players_raw <- bind_rows(lapply(players_raw, as.data.frame.list))
  players_data <-players_raw[,c(
    'feed_id',
    'id',
    'first_name',
    'last_name',
    'team.abbrev',
    'positions.position',
    'positions.position.1',
    'player_stats.ppts',
    'player_match_stats.points'
  )]
  colnames(players_data) <- c(
    'feed_id',
    'player_id',
    'first_name',
    'last_name',
    'team_abbrev',
    'pos',
    'pos2',
    'projected_points',
    'points'
  )
  
  # PLAYER TO TEAM LINK  ------
  league_raw <- readRDS(paste0('./references/data/raw/2020_',rnd_str,'_SC_TEAMS.RDS'))
  
  league_data <- tibble()
  for(tm in 1:length(league_raw$ladder)){
    
    team_raw <- append(
      league_raw$ladder[[tm]]$userTeam$scores$scoring,
      league_raw$ladder[[tm]]$userTeam$scores$nonscoring
    )
    
    team_raw <- lapply(team_raw, unlist)
    team_raw <- bind_rows(lapply(team_raw, as.data.frame.list))
    team_data <-team_raw[,c(
      'player_id',
      'user_team_id',
      'picked',
      'position'
    )]
    
    league_data <- bind_rows(league_data, team_data)

  }
  
  ## TEAM DATA  ------
  
  ladder_raw <- lapply(league_raw$ladder, unlist)
  ladder_raw <- bind_rows(lapply(ladder_raw, as.data.frame.list))
  ladder_data <-ladder_raw[,c(
    'user_team_id',
    'userTeam.teamname',
    'userTeam.user.first_name'
  )]
  colnames(ladder_data) <- c(
    'user_team_id',
    'sc_team',
    'sc_coach'
  )
  
  ## COMBINE DATA ------
  round_data <- left_join(players_data, league_data, by=c('player_id'))
  round_data <- left_join(round_data, ladder_data, by=c('user_team_id'))
  
  round_data <- round_data %>%
    add_column(round = rnd) %>%
    add_column(year = 2020) %>%
    mutate(projected_points = as.numeric(projected_points)) %>%
    mutate(points = as.numeric(points))
    
  
  master_data <- bind_rows(master_data, round_data)
  
}

write_csv(master_data, './references/data/clean/Master_Player_Data_2020.csv', na='')
