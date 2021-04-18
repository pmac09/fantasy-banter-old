sc_get_league_data <- function(auth_headers, round){
  
  # pull teams for current round
  data <- content(GET(
    url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/leagues/96/ladderAndFixtures?round=',round,'&scores=true'),
    config = auth_headers
  ))
  
  return(data)
  
}

