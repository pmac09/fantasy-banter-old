# Load supercoach functions
source('./references/functions/load.R')

# Refresh supercoach data
refresh_sc_data(cid, tkn)

# Get current round
auth_headers <- get_sc_auth(cid, tkn)
settings <- get_sc_settings(auth_headers)
rnd <- settings$competition$current_round

master_data <- tibble()
for(i in 1:rnd){
  
  message('Round ', i, '...')
  rnd_str <- sprintf("%02d", i)

  # Player stats
  player_raw <- readRDS(paste0('./references/data/raw/2020_',rnd_str,'_SC_PLAYER_DATA.RDS'))
  player_data <- get_sc_player_data(player_raw)

  # SC Lineups
  league_raw <- readRDS(paste0('./references/data/raw/2020_',rnd_str,'_SC_LEAGUE_DATA.RDS'))
  team_data <- get_sc_team_data(league_raw) 
  
  # Combine
  round_data <- left_join(player_data, team_data, by=c('player_id')) %>%
    add_column(round = rnd) %>%
    add_column(year = 2020) %>%
    mutate(projected_points = as.numeric(projected_points)) %>%
    mutate(points = as.numeric(points))
  
  # Merge back to master
  master_data <- bind_rows(master_data, round_data)
}

#Add team names
ladder_data <- get_sc_ladder_data(league_raw)[,c(1:3)]
master_data <- left_join(master_data, ladder_data, by=c('user_team_id'))

# Save .csv
write_csv(master_data, './references/data/clean/2020_Player_Data.csv', na='')
