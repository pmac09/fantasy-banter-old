get_selected_game <- function(fixture_data, selection){
  
  selected_game <- fixture_data %>%
    filter(matchup == selection)
  
  return(selected_game)
  
}

