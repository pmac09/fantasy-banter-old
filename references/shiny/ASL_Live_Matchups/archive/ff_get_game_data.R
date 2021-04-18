ff_get_game_data <- function(game_id){
  
  url <- paste0("http://live.fanfooty.com.au/chat/", game_id, ".txt")
  game_data <- strsplit(readLines(url),",")
  
  game_data <- as.data.frame(t(as.data.frame(game_data[5:length(game_data)])))
  row.names(game_data) <- NULL
  colnames(game_data) <- c(
    'player.feed_id',
    'player.first_name',
    'player.last_name',
    'player.team.abbrev',
    'disposals',
    'dreamteam',
    'supercoach',
    8,
    9,
    10,
    'kicks',
    'handballs',
    'marks',
    'tackles',
    'hitouts',
    'frees_for',
    'frees_against',
    'goals',
    'behinds',
    'gametime',
    'icon_1',
    'desc_1',
    'icon_2',
    'desc_2',
    25,
    26,
    27,
    28,
    'position',
    'jersey',
    31,
    32,
    33,
    34,
    35,
    36,
    37,
    38,
    39,
    'contested_possessions',
    'clearances',
    'clangers',
    'disposal_effeciency',
    'time_on_ground',
    'metres_gained'
  )
  
  return(game_data)
  
}
