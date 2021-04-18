sc_get_team_data <- function(sc_data){
  
  team_data <- tibble()
  for (nTeam in 1:length(sc_data$ladder)){
    
    players <- append(
      sc_data$ladder[[nTeam]]$userTeam$scores$scoring,
      sc_data$ladder[[nTeam]]$userTeam$scores$nonscoring
    )
    
    players <- lapply(players, unlist)
    players <- bind_rows(lapply(players, as.data.frame.list))
    
    team_data <- bind_rows(team_data, players)
    
  }
  
  team_data <- team_data[,c('user_team_id',
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
    arrange(user_team_id, player.played_status.status, player.team.abbrev)
  
  return(team_data)
  
}
