refresh_sc_data <- function(cid, tkn, rnd = NULL){
  
  path <- paste0('./references/data/raw/')
  
  auth_headers <- get_sc_auth(cid, tkn)
  settings <- get_sc_settings(auth_headers)
  
  if(is.null(rnd)){
    rnd <- settings$competition$current_round
  }
  
  # Download selected round details
  message(paste0('Downloading round ', rnd ,' data...'))
  
  league_data <- get_sc_league_raw(auth_headers, rnd)
  saveRDS(league_data, file=paste0(path, sprintf("2020_%02d", rnd), '_SC_LEAGUE_DATA.RDS'))
  
  player_data <- get_sc_player_raw(auth_headers, rnd)
  saveRDS(player_data, file=paste0(path, sprintf("2020_%02d", rnd), '_SC_PLAYER_DATA.RDS'))
  
  # Download live round details
  nxt_rnd <- settings$competition$next_round
  message(paste0('Downloading round ', nxt_rnd,'(live) data...'))
  
  league_data <- get_sc_league_raw(auth_headers, nxt_rnd)
  saveRDS(league_data, file=paste0(path, '2020_LIVE_SC_LEAGUE_DATA.RDS'))
  
  player_data <- get_sc_player_raw(auth_headers, nxt_rnd)
  saveRDS(player_data, file=paste0(path, '2020_LIVE_SC_PLAYER_DATA.RDS'))
  
}
