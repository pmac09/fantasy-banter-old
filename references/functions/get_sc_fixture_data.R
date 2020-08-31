get_sc_fixture_data <- function(league_raw){
  
  fixture <- lapply(league_raw$fixtures, unlist)
  fixture <- bind_rows(lapply(fixture, as.data.frame.list))
  
  home_data <- fixture %>%
    mutate(year = 2020) %>%
    rename(team_id = user_team1.id,
           team = user_team1.teamname,
           coach = user_team1.user.first_name,
           opponent_team_id = user_team2.id,
           opponent_team = user_team2.teamname,
           opponent_coach = user_team2.user.first_name
           )
  
  home_data <- home_data[,c(
    'year',
    'round',
    'fixture',
    'team_id',
    'team',
    'coach',
    'opponent_team_id',
    'opponent_team',
    'opponent_coach'
  )]
  
  away_data <- home_data[,c(
    'year',
    'round',
    'fixture',
    'opponent_team_id',
    'opponent_team',
    'opponent_coach',
    'team_id',
    'team',
    'coach'
  )]
  
  names(away_data) <- names(home_data)
  
  fixture_data <- bind_rows(home_data, away_data) %>%
    mutate(team_id = as.numeric(team_id)) %>%
    mutate(opponent_team_id = as.numeric(opponent_team_id)) %>%
    mutate(round = as.numeric(round))
  
  return(fixture_data)
  
}
