#---------------------------------------------------------
# Required packages
options(stringsAsFactors = FALSE)
library(httr)
library(tidyverse)

#---------------------------------------------------------
# Function for getting auth headers to make HTTP requests to supercoach
get_auth <- function(cid, tkn){
  
  # POST request to get an access token
  auth <- content(POST(
    url = 'https://supercoach.heraldsun.com.au/2020/api/afl/classic/v1/access_token',
    body = list(
      grant_type = 'social',
      client_id = cid,
      client_secret = '',
      service = 'auth0',
      token = tkn
    ),
    encode = 'json'
  ))
  
  headers <- add_headers(
    Authorization = paste0('Bearer ', auth$access_token)
  )
  
  return(headers)
}

#---------------------------------------------------------
# Function to download and save raw data from supercoach 
download_supercoach <- function(rnd=1, auth_headers){

  filepath <- paste0('./references/data/raw/',sprintf("2020_%02d", rnd))
  
  players <- content(GET(
    url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=',rnd),
    config = auth_headers
  ))
  
  saveRDS(players, file=paste0(filepath, '_SC_PLAYERS.RDS'))
  

  teams <- content(GET(
    url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/leagues/96/ladderAndFixtures?round=',rnd,'&scores=true'),
    config = auth_headers
  ))
  
  saveRDS(teams, file=paste0(filepath, '_SC_TEAMS.RDS'))
}

#---------------------------------------------------------
# Function to refresh master data
refresh_supercoach <- function(){
  
  files <- list.files('./references/data/raw/')
  files <- unique(substr(files, 1,7))
  
  master_data <- tibble()
  
  for (file in files){
    
    filename <- paste0('./references/data/raw/', file, '_SC_TEAMS.RDS')
    
    data <- readRDS(filename)
    
    league <- lapply(data$ladder, unlist)
    league <- bind_rows(lapply(league, as.data.frame.list))
    
    league <- league[,c(
      'user_team_id',
      'userTeam.teamname',
      'userTeam.user_id',
      'userTeam.user.first_name'
    )]
  
      
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
    
    team_data <- team_data[,c(
      'player.feed_id',
      'player_id', 
      'round',
      'picked',
      'position',
      'points',
      'ppts',
      'player.first_name',
      'player.last_name',
      'player.team.abbrev',
      'user_team_id'
    )]
    
    team_data2 <- left_join(team_data, league, by=c('user_team_id'))
    
    master_data <- bind_rows(master_data, team_data2)  
    
  }
  
  write_csv(master_data, paste0('./references/data/clean/2020_Master_Data.csv'))
  
}



