get_sc_fixture_data <- function(league_raw){
  
  fixture <- lapply(league_raw$fixtures, unlist)
  fixture <- bind_rows(lapply(fixture, as.data.frame.list))
  fixture <- fixture[, c('round',
                         'fixture',
                         'user_team1.id',
                         'user_team1.teamname',
                         'user_team1.user.first_name',
                         'user_team2.id',
                         'user_team2.teamname',
                         'user_team2.user.first_name')]
  
  fixture <- fixture %>%
    mutate(matchup = paste0(user_team1.teamname, ' vs ', user_team2.teamname))
  
  return(fixture)
  
}
