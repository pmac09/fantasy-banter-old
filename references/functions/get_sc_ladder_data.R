get_sc_ladder_data <- function(league_raw){
  
  ladder_raw <- league_raw$ladder
  
  ladder_raw <- lapply(ladder_raw, unlist)
  ladder_raw <- bind_rows(lapply(ladder_raw, as.data.frame.list))

  ladder_data <- ladder_raw[,c(
    'user_team_id',
    'userTeam.teamname',
    'userTeam.user.first_name',
    'round',
    'wins',
    'draws',
    'losses',
    'points',
    'points_for',
    'points_against',
    'position',
    'userTeam.stats.points',
    'userTeam.stats.total_points'
  )] %>%
    rename(teamname = userTeam.teamname,
           coach = userTeam.user.first_name,
           round_points = userTeam.stats.points,
           total_points = userTeam.stats.total_points)

  return(ladder_data)

}
