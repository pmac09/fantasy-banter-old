ff_get_fixture_data <- function(live=FALSE){

  fixture <- fread('http://www.fanfooty.com.au/resource/draw.php')
  colnames(fixture) <- c('game_id', 
                         'year', 
                         'competition', 
                         'round', 
                         'gametime_AET', 
                         'day', 
                         'home_team', 
                         'away_team', 
                         'ground', 
                         'timeslot', 
                         'TV_coverage', 
                         'home_supergoals', 
                         'home_goals', 
                         'home_behinds', 
                         'home_points', 
                         'away_supergoals', 
                         'away_goals', 
                         'away_behinds', 
                         'away_points', 
                         'match_status')
  
  if(live==TRUE){
    fixture <- fixture %>%
      filter(fixture$gametime_AET == Sys.Date())
  }
  
  return(fixture)
}
