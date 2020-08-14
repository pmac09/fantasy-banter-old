get_sc_player_data <- function(player_raw){
  
  player_raw <- lapply(player_raw, unlist)
  player_raw <- bind_rows(lapply(player_raw, as.data.frame.list))
  player_data <- player_raw[,c(
    'feed_id',
    'id',
    'first_name',
    'last_name',
    'team.abbrev',
    'positions.position',
    'positions.position.1',
    'player_stats.ppts',
    'player_match_stats.points'
  )]
  
  colnames(player_data) <- c(
    'feed_id',
    'player_id',
    'first_name',
    'last_name',
    'team_abbrev',
    'pos',
    'pos2',
    'projected_points',
    'points'
  )
  
  player_data <- player_data %>%
    mutate(projected_points = as.numeric(projected_points)) %>%
    mutate(points = as.numeric(points))
  
  return(player_data)
  
}
