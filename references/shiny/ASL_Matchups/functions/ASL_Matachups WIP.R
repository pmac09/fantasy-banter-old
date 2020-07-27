# Working file for ASL Matchups Shiny app

#--------------------------------------------------------------
# SETUP
setwd('./references/shiny/ASL_Matchups')

options(stringsAsFactors = FALSE)

library(tidyverse)
library(httr)

#--------------------------------------------------------------
# Functions
source('./functions/secrets.R')
source('./functions/get_auth.R')


#--------------------------------------------------------------
# START

# create authentication
auth_headers <- get_auth(cid, tkn)

# get game settings
settings <- content(GET(
  url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/settings'),
  config = auth_headers
))

# extract current round
rnd <- settings$competition$next_round


get_supercoach_data <- function(auth_headers, round){
  
  # pull teams for current round
  data <- content(GET(
    url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/leagues/96/ladderAndFixtures?round=',round,'&scores=true'),
    config = auth_headers
  ))
  
  return(data)
  
}









# Extract player info

team_data <- tibble()
for (nTeam in 1:length(data$ladder)){
  
  players <- append(
    data$ladder[[nTeam]]$userTeam$scores$scoring,
    data$ladder[[nTeam]]$userTeam$scores$nonscoring
  )
  
  players <- lapply(players, unlist)
  players <- bind_rows(lapply(players, as.data.frame.list))
  
  team_data <- bind_rows(team_data, players)
  
}

user_team <- team_data[,c('user_team_id',
                          'player_id',
                          'player.feed_id',
                          'player.first_name',
                          'player.last_name',
                          'position',
                          'player.team.abbrev',
                          'picked',
                          'position_sort',
                          'player.player_stats.avg',
                          'points',
                          'ppts')] %>%
  mutate(player.player_stats.avg = as.numeric(player.player_stats.avg)) %>%
  filter(user_team_id==33301) %>%
  filter(picked=='true') %>%
  arrange(position_sort, desc(player.player_stats.avg))


#-----------------------------------
# get fanfooty match info

library(data.table)

fixture <- fread('http://www.fanfooty.com.au/resource/draw.php')
colnames(fixture) <- c('game_id', 
                        'year', 
                        'competition', 
                        'round', 
                        'gametime_AET', 
                        'day', 
                        'home_team', 
                        'away_team', 
                        'ground', 
                        'timeslot', 
                        'TV_coverage', 
                        'home_supergoals', 
                        'home_goals', 
                        'home_behinds', 
                        'home_points', 
                        'away_supergoals', 
                        'away_goals', 
                        'away_behinds', 
                        'away_points', 
                        'match_status')

fixture <- fixture %>%
  filter(fixture$gametime_AET == Sys.Date()) %>%
  filter(match_status == '')

live_fixture <- fixture %>%
  filter(fixture$gametime_AET <= Sys.time())


gameNo <- live_fixture$game_id

url <- paste0("http://live.fanfooty.com.au/chat/", gameNo, ".txt")
game_data <- strsplit(readLines(url),",")
  
game_data <- t(as.data.frame(game_data[5:length(game_data)])) 
row.names(game_data) <- NULL
colnames(game_data) <- c(
  'player.feed_id',
  'player.first_name',
  'player.last_name',
  'player.team.abbrev',
  'disposals',
  'dreamteam',
  'supercoach',
  8,
  9,
  10,
  'kicks',
  'handballs',
  'marks',
  'tackles',
  'hitouts',
  'frees_for',
  'frees_against',
  'goals',
  'behinds',
  'gametime',
  'icon_1',
  'desc_1',
  'icon_2',
  'desc_2',
  25,
  26,
  27,
  28,
  'position',
  'jersey',
  31,
  32,
  33,
  34,
  35,
  36,
  37,
  38,
  39,
  'contested_possessions',
  'clearances',
  'clangers',
  'disposal_effeciency',
  'time_on_ground',
  'metres_gained'
)



user_team




#--------------------------------------
# ON START UP

source('./functions/secrets.R')
source('./functions/get_auth.R')
source('./functions/get_supercoach_data.R')
source('./functions/get_fixture_data.R')
source('./functions/get_sc_teams.R')
source('./functions/get_selected_game.R')
source('./functions/get_selected_team.R')


# Get authentication 
auth_headers <- get_auth(cid, tkn)

# Get game settings
settings <- content(GET(
  url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/settings'),
  config = auth_headers
))

round <- settings$competition$next_round

sc_data <- get_supercoach_data(auth_headers, round)

fixture_data <- get_fixture_data(sc_data)

sc_teams <- get_sc_teams(sc_data)

input.lstMatchup <- fixture_data$matchup[1]

selected_game <- get_selected_game(fixture_data, input.lstMatchup)

selected_team <- get_selected_team(sc_teams, selected_game, TRUE)


                        
