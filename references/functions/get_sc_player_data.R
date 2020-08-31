get_sc_player_data <- function(player_raw){
  
  player_raw <- lapply(player_raw, unlist)
  player_raw <- bind_rows(lapply(player_raw, as.data.frame.list))
  
  player_data <- tibble(
    feed_id          = as.numeric(player_raw$feed_id),
    player_id        = as.numeric(player_raw$id),
    first_name       = player_raw$first_name,
    last_name        = player_raw$last_name,
    team_abbrev      = player_raw$team.abbrev,
    pos              = player_raw$positions.position,
    pos2             = player_raw$positions.position.1,
    projected_points = rep(NA, length(player_raw$feed_id)),
    points           = as.numeric(player_raw$player_stats.points)
  )
  
  if('player_stats.ppts' %in% names(player_raw)){
    player_data$projected_points = as.numeric(player_raw$player_stats.ppts)
  }
  
  return(player_data)
}
