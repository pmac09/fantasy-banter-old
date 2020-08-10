get_sc_league_raw <- function(auth_headers, round, league_num=96){
  
  url <- paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/leagues/',league_num,'/ladderAndFixtures?round=',round,'&scores=true')
  
  # pull teams for current round
  data <- content(GET(
    url = url,
    config = auth_headers
  ))
  
  return(data)
  
}




