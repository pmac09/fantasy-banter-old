get_selected_team <- function(sc_teams, selected_game, home_flag = TRUE){
  
  if(home_flag){
    team_id <- selected_game$user_team1.id
  } else {
    team_id <- selected_game$user_team2.id
  }
  
  selected_team <- sc_teams %>%
    filter(user_team_id == team_id) %>%
    mutate(player = paste0(substr(player.first_name,1,1), '.', player.last_name) %>%
    select(player, position)
  
  
  
  return(selected_team)
}
