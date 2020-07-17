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
  
  save(players, file=paste0(filepath, '_SC_PLAYERS.RData'))
  

  teams <- content(GET(
    url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/leagues/96/ladderAndFixtures?round=',rnd,'&scores=true'),
    config = auth_headers
  ))
  
  save(teams, file=paste0(filepath, '_SC_TEAMS.RData'))
}

