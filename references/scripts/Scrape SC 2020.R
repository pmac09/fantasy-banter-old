options(stringsAsFactors = FALSE)

library(httr)
library(tidyverse)

source('./references/functions/secrets.R')
source('./references/functions/scrape_supercoach.R')

# Get authentication
auth_headers <- get_auth(cid, tkn)

# Download Data
for(i in 1:6){
  download_supercoach(i, auth_headers)
}

team_data <- tibble()

for (nTeam in 1:length(raw$ladder)){
  
  players <- append(
    raw$ladder[[nTeam]]$userTeam$scores$scoring,
    raw$ladder[[nTeam]]$userTeam$scores$nonscoring
  )

  players <- lapply(players, unlist)
  players <- bind_rows(lapply(players, as.data.frame.list))

  team_data <- bind_rows(team_data, players)
    
}

fileName <- paste0('./references/data/raw/',sprintf("2020_%02d", rnd),'_SUPERCOACH_RAW.csv')
write_csv(team_data, fileName, na="")




# df <- content(GET(
#   url = 'https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=5',
#   config = headers
# ))










