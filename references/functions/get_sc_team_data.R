get_sc_team_data <- function(league_raw){
  
  team_data <- tibble()
  
  for (nTeam in 1:length(league_raw$ladder)){
    
    players <- append(
      league_raw$ladder[[nTeam]]$userTeam$scores$scoring,
      league_raw$ladder[[nTeam]]$userTeam$scores$nonscoring
    )
    
    players <- lapply(players, unlist)
    players <- bind_rows(lapply(players, as.data.frame.list))
    
    team_data <- bind_rows(team_data, players)
    
  }
  
  team_data <- team_data[,c(
    'player_id',
    'user_team_id',
    'picked',
    'position'
  )]
  
  return(team_data)

}
