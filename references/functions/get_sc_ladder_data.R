get_sc_ladder_data <- function(league_raw){
  
  ladder_raw <- league_raw$ladder
  
  ladder_raw <- lapply(ladder_raw, unlist)
  ladder_raw <- bind_rows(lapply(ladder_raw, as.data.frame.list))

  ladder_data <- tibble(
    user_team_id   = as.numeric(ladder_raw$user_team_id),
    teamname       = ladder_raw$userTeam.teamname,
    coach          = ladder_raw$userTeam.user.first_name,
    round          = as.numeric(ladder_raw$round),
    wins           = as.numeric(ladder_raw$wins),
    draws          = as.numeric(ladder_raw$draws),
    losses         = as.numeric(ladder_raw$losses),
    points         = as.numeric(ladder_raw$points),
    points_for     = as.numeric(ladder_raw$points_for),
    points_against = as.numeric(ladder_raw$points_against),
    position       = as.numeric(ladder_raw$position),
    round_points   = as.numeric(ladder_raw$userTeam.stats.points),
    total_points   = as.numeric(ladder_raw$userTeam.stats.total_points)
  )

  return(ladder_data)
}
