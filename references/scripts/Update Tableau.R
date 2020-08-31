# Load supercoach functions
source('./references/functions/load.R')

library(googlesheets4)

auth_headers <- get_sc_auth(cid, tkn)
settings <- get_sc_settings(auth_headers)
rnd <- settings$competition$next_round

player_raw <- get_sc_player_raw(auth_headers, rnd)
player_data <- get_sc_player_data(player_raw)

league_raw <- get_sc_league_raw(auth_headers, rnd)
team_data <- get_sc_team_data(league_raw) 

# Combine
round_data <- left_join(player_data, team_data, by=c('player_id')) %>%
  add_column(round = rnd) %>%
  add_column(year = 2020) %>%
  mutate(player = paste0(substr(first_name,1,1),'.',last_name))

## PROJ OVERRIDE
if(is.na(round_data$projected_points[1])) {
  
  master_data <- read_csv('./references/data/clean/2020_Player_Data.csv') 
  
  rd_proj <- master_data[,c('player_id', 'points')] %>%
    filter(!is.na(points)) %>%
    filter(points>0) %>%
    group_by(player_id) %>%
    summarise(
      proj = median(points, na.rm=T),
      .groups = 'drop'
    ) 
  
  round_data <- left_join(round_data, rd_proj, by=c('player_id')) %>%
    mutate(projected_points = ifelse(is.na(projected_points), proj, projected_points))
  
  byes <- c('GCS','NTH','PTA','WBD','GEE','STK')
  #'R16' = c('COL','RIC')
  
  round_data <- round_data[,!(names(round_data) %in% c('proj'))] %>%
    mutate(projected_points = ifelse(team_abbrev %in% byes, 0 , projected_points))
  
}

round_data <- round_data  %>%
  mutate(live = ifelse(points == 0, projected_points, points))



gs <- 'https://docs.google.com/spreadsheets/d/1FnFhEs_irZIT-hbkTnVA0HxyYftIbxPUISHgNq1escc'

write_sheet(round_data, ss = gs, sheet = 'Players')



