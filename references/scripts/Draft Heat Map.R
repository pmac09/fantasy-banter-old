
options(stringsAsFactors = FALSE)

library(httr)
library(tidyverse)

source('./references/functions/secrets.R')
source('./references/functions/scrape_supercoach.R')


## Get Draft Data

# auth_headers <- get_auth(cid, tkn)
# 
# draft_data <- content(GET(
#   url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/leagues/96/recap'),
#   config = auth_headers
# ))
#   
# saveRDS(draft_data, '../../references/data/raw/2020_00_SC_DRAFT.RDS')



draft_raw<- readRDS( './references/data/raw/2020_00_SC_DRAFT.RDS')
draft_raw <- lapply(draft_raw, unlist)
draft_raw <- bind_rows(lapply(draft_raw, as.data.frame.list))
draft_data <- as_tibble(draft_raw[,c(
  'user_team_id',
  'round',
  'pick',
  'player_id'
)]) %>%
  mutate(pick = as.numeric(pick)) %>%
  mutate(round = as.numeric(round))


rnd <- 8
rnd_str <- sprintf("%02d", rnd)

players_raw <- readRDS(paste0('./references/data/raw/2020_',rnd_str,'_SC_PLAYERS.RDS'))
players_raw <- lapply(players_raw, unlist)
players_raw <- bind_rows(lapply(players_raw, as.data.frame.list))
player_data <- as_tibble(players_raw[,c(
  'id',
  'first_name',
  'last_name',
  'team.abbrev',
  'positions.position',
  'positions.position.1',
  'player_stats.avg'
)]) %>%
  mutate(player_stats.avg = as.numeric(player_stats.avg))

# Get median
median_data <- tibble()
for(i in 1:rnd){
  rnd_str <- sprintf("%02d", i)
  players_raw <- readRDS(paste0('./references/data/raw/2020_',rnd_str,'_SC_PLAYERS.RDS'))
  players_raw <- lapply(players_raw, unlist)
  players_raw <- bind_rows(lapply(players_raw, as.data.frame.list))
  median_raw <- as_tibble(players_raw[,c(
    'id',
    'player_match_stats.points'
  )]) %>%
    mutate(player_match_stats.points = as.numeric(player_match_stats.points))
  
  median_data <- bind_rows(median_data, median_raw)
}

median_data <- median_data %>%
  group_by(id) %>%
  summarise(median_score = median(player_match_stats.points, na.rm=T)) %>%
  arrange(desc(median_score))

median_data$median_score[is.na(median_data$median_score)] <- 0


team_raw <- readRDS(paste0('./references/data/raw/2020_',rnd_str,'_SC_TEAMS.RDS'))
team_data <- tibble()
for (nTeam in 1:length(team_raw$ladder)){
  
  players <- append(
    team_raw$ladder[[nTeam]]$userTeam$scores$scoring,
    team_raw$ladder[[nTeam]]$userTeam$scores$nonscoring
  )
  
  players <- lapply(players, unlist)
  players <- bind_rows(lapply(players, as.data.frame.list))
  
  team_data <- bind_rows(team_data, players)
}

team_data <- team_data[,c(
  'player_id',
  'user_team_id'
)] %>%
  rename(current_team_id = user_team_id)

heat_map <- left_join(player_data, median_data, by=c('id'))
heat_map <- left_join(heat_map, draft_data, by=c('id'='player_id'))
heat_map <- left_join(heat_map, team_data, by=c('id'='player_id'))



#DeGoey no in the draft data
degoey <- heat_map$id==148
heat_map[degoey,]$user_team_id <- 186
heat_map[degoey,]$round <- 12
heat_map[degoey,]$pick <- 89

# Fix the draft order to suit ours
heat_map <- heat_map %>% 
  arrange(pick) %>% 
  add_column(fix_order = NA)
heat_map$fix_order[1:176] <- c(1:8, rep(c(1:8, 8:1), 10), 1:8)
heat_map <- heat_map %>% arrange(round, fix_order) 
heat_map$pick[1:176] <- c(1:176)
heat_map <- heat_map[,-which(names(heat_map) %in% c('fix_order'))]
heat_map$positions.position.1[is.na(heat_map$positions.position.1)] <- ''

# Create positional flags
positions <- c('DEF','MID','RUC','FWD')
pos_count <- c(48, 64, 12, 48)

for (i in 1:4){
  
  pos <- positions[i]
  
  pos_data <- heat_map %>%
    filter(positions.position == pos | positions.position.1 == pos) %>%
    arrange(desc(median_score))
    
  mean <- mean(pos_data$median_score[1:pos_count[i]])
  stdev <- sd(pos_data$median_score[1:pos_count[i]])
  
  pos_data <- pos_data %>%
    mutate(norm_score = (median_score-mean)/(stdev))
  
  pos_data <- pos_data[,c('id','norm_score')]
  colnames(pos_data)[2] <- pos
  
  heat_map <-left_join(heat_map, pos_data, by=c('id'))
}

heat_map <- heat_map %>%
  rowwise() %>%
  mutate(score = round(max(DEF,MID,RUC,FWD, na.rm=T),4)) %>%
  arrange(pick)

write.csv(heat_map, './references/data/clean/draft_heat_map.csv', na='', row.names=F)



