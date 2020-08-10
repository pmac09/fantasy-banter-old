get_sc_player_raw <- function(auth_headers, round){
  
  url <- paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=',round)

  # pull teams for current round
  data <- content(GET(
    url = url,
    config = auth_headers
  ))
  
  return(data)
  
}
