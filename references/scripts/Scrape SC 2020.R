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






