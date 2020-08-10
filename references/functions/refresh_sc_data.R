refresh_sc_data <- 
  
  function(cid, tkn){
  
  path <- paste0('./references/data/raw/')
  
  auth_headers <- get_sc_auth(cid, tkn)
  settings <- get_sc_settings(auth_headers)
  rnd <- settings$competition$current_round
  
  for(i in 1:rnd){
    
    message(paste0('Downloading round ', i ,' data...'))
    
    league_data <- get_sc_league_raw(auth_headers, i)
    saveRDS(league_data, file=paste0(path, sprintf("2020_%02d", i), '_SC_LEAGUE_DATA.RDS'))

    player_data <- get_sc_player_raw(auth_headers, i)
    saveRDS(player_data, file=paste0(path, sprintf("2020_%02d", i), '_SC_PLAYER_DATA.RDS'))
    
  }
  
}