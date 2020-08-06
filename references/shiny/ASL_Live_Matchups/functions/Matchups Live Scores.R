# Working file for ASL Matchups Shiny app

#--------------------------------------------------------------
# SETUP
options(stringsAsFactors = FALSE)

library(tidyverse)
library(httr)

#--------------------------------------------------------------
# Functions
source('./references/functions/secrets.R')
source('./references/functions/scrape_supercoach.R')


get_supercoach_data <- function(auth_headers, round){
  
  # pull teams for current round
  data <- content(GET(
    url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/leagues/96/ladderAndFixtures?round=',round,'&scores=true'),
    config = auth_headers
  ))
  
  return(data)
  
}



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

# Get data
sc_raw <- get_supercoach_data(auth_headers, rnd)

# Extract player info
team_data <- tibble()
for (nTeam in 1:length(sc_raw$ladder)){
  
  players <- append(
    sc_raw$ladder[[nTeam]]$userTeam$scores$scoring,
    sc_raw$ladder[[nTeam]]$userTeam$scores$nonscoring
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
                          'ppts',
                          'player.played_status.status')] %>%
  mutate(player.player_stats.avg = as.numeric(player.player_stats.avg)) %>%
  filter(picked=='true') %>%
  arrange(position_sort, desc(player.player_stats.avg))


# Print game list
for(i in 1:length(sc_raw$fixtures)){
  team1 <- sc_raw$fixtures[[i]]$user_team1$teamname
  team2 <- sc_raw$fixtures[[i]]$user_team2$teamname
  print(paste0(i,': ',team1, ' vs ', team2))
}

gme <- 1











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

setwd('./references/shiny/ASL_Matchups')

options(stringsAsFactors = FALSE)

library(tidyverse)
library(httr)
library(data.table)

source('./functions/secrets.R')
source('./functions/get_auth.R')

source('./functions/sc_get_league_data.R')
source('./functions/sc_get_fixture_data.R')
source('./functions/sc_get_team_data.R')

source('./functions/ff_get_fixture.R')  
source('./functions/ff_get_game_data.R')  
source('./functions/ff_get_live_games.R')  


# Get authentication 
auth_headers <- get_auth(cid, tkn)

# Get game settings
settings <- content(GET(
  url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/settings'),
  config = auth_headers
))

rnd <- settings$competition$next_round # Get current round

league_data <- sc_get_league_data(auth_headers, rnd) # League data for current round
team_data <- sc_get_team_data(league_data) # Teams for the selected round
fixture_data <- sc_get_fixture_data(league_data) # Fixture for the selected round
selected_fixture <- fixture_data %>% filter(matchup == fixture_data$matchup[1]) # Selected game from fixture

ff_fixture <- ff_get_fixture(live = TRUE) # Today's games via fanfooty

lData <- ff_live_data(ff_fixture)


tData <- team_data[,c(
  'user_team_id',
  'player.feed_id',
  'player.first_name',
  'player.last_name',
  'player.played_status.status',
  'points'
)]

lData <- ff_live_data[,c(
  'player.feed_id',
  'supercoach'
)]

projection_data <- left_join(tData, lData, by=c('player.feed_id'))










