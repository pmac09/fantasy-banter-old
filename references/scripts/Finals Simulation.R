# Load supercoach functions
source('./references/functions/load.R')

master_data <- read_csv('./references/data/clean/2020_Player_Data.csv') 
  
team_data <- master_data %>%
  filter(picked == TRUE) %>%
  group_by(round, user_team_id) %>%
  summarise(
    points = sum(points, na.rm=T),
    .groups = 'drop'
    ) 

player_data <- master_data %>%
  group_by(player_id, last_name, team_abbrev) %>%
  filter(points > 0) %>%
  summarise(
    mean = mean(points, na.rm=T),
    sd = sd(points, na.rm=T),
    min = min(points, na.rm=T),
    .groups = 'drop'
  )

missing_data <- master_data %>%
  rowwise() %>%
  mutate(points = max(points, projected_points, na.rm=T)) %>%
  ungroup() %>%
  group_by(player_id, last_name, team_abbrev) %>%
  filter(points > 0) %>%
  summarise(
    mean = mean(projected_points, na.rm=T),
    sd = sd(projected_points, na.rm=T),
    min = min(points, na.rm=T),
    .groups = 'drop'
  )



## Get Current Teams
auth_headers <- get_sc_auth(cid, tkn)
settings <- get_sc_settings(auth_headers)
rnd <- settings$competition$next_round
live_raw <- get_sc_league_raw(auth_headers, rnd)
live_data <- get_sc_team_data(live_raw) 

live_teams <- left_join(live_data, player_data, by=c('player_id')) %>%
  filter(picked == TRUE)

miss <- live_teams$player_id[is.na(live_teams$sd)]
miss <- inner_join(live_data, missing_data[which(missing_data$player_id == miss),], by=c('player_id')) %>%
  filter(picked == TRUE)

live_teams <- live_teams[!is.na(live_teams$sd),]
live_teams <- bind_rows(live_teams, miss)



## GET FIXTURE ------------------

# fixture <- tibble() 
# for(i in 1:16){
#   
#   league_raw <- get_sc_league_raw(auth_headers, i)
#   fixture_data <- get_sc_fixture_data(league_raw)
#   
#   fixture <- bind_rows(fixture, fixture_data)
# }


fixture <- read_csv('./references/data/clean/2020_Fixture_Data.csv') %>%
  filter(round > 16) 

## START SIM ------------------

list.coach <- unique(fixture$coach)

noOfSims <- 10000

results <- tibble()
for( sim in 1:noOfSims){
  
  data <- fixture[1:9]
  
  ## Simulate team
  sim_teams <- tibble()
  for(i in 17:18){
    
    proj <- live_teams %>%
      rowwise() %>%
      mutate(points = round(rnorm(1, mean, sd),0)) %>%
      mutate(points = ifelse(points<0, min, points))
    
    proj_teams <- proj %>%
      group_by(user_team_id) %>%
      summarise(
        points = sum(points),
        .groups = 'drop'
      ) %>%
      mutate(round = i)
    
    sim_teams <- bind_rows(sim_teams, proj_teams)
    
  }
  
  names(sim_teams)[grep('points', names(sim_teams))] <- 'home_points'
  data <- left_join(data, sim_teams, by=c('round', 'team_id'='user_team_id'))
  
  names(sim_teams)[grep('points', names(sim_teams))] <- 'away_points'
  data <- left_join(data, sim_teams, by=c('round', 'opponent_team_id'='user_team_id'))
  

  tmp <- data %>%
    mutate(DIFFERENTIAL = home_points - away_points) %>%
    mutate(WIN = ifelse(DIFFERENTIAL>0, 1, 0)) %>%
    mutate(DRAW = ifelse(DIFFERENTIAL==0, 1, 0)) %>%
    mutate(LOSS = ifelse(DIFFERENTIAL<0, 1, 0)) 
  
  gf <- tmp %>%
    filter(WIN == 1)
  
  data[data$round == 18, 4:6][1,] <- gf[1,4:6]
  data[data$round == 18, 7:9][2,] <- gf[1,4:6]

  data[data$round == 18, 4:6][2,] <- gf[2,4:6]
  data[data$round == 18, 7:9][1,] <- gf[2,4:6]
  
  names(sim_teams)[grep('points', names(sim_teams))] <- 'home_points'
  data <- left_join(data[,1:9], sim_teams, by=c('round', 'team_id'='user_team_id'))
  
  names(sim_teams)[grep('points', names(sim_teams))] <- 'away_points'
  data <- left_join(data, sim_teams, by=c('round', 'opponent_team_id'='user_team_id'))
  
  tmp <- data %>%
    mutate(DIFFERENTIAL = home_points - away_points) %>%
    mutate(WIN = ifelse(DIFFERENTIAL>0, 1, 0)) %>%
    mutate(DRAW = ifelse(DIFFERENTIAL==0, 1, 0)) %>%
    mutate(LOSS = ifelse(DIFFERENTIAL<0, 1, 0)) 
  
  tmp <- tmp %>%
    group_by(team) %>%
    summarise(
      W = sum(WIN),
      D = sum(DRAW),
      L = sum(LOSS),
      PF = sum(home_points),
      PA = sum(away_points), 
      .groups = 'drop'
    ) %>%
    ungroup() %>%
    mutate(PTS = (W*4)+(D*2)) %>%
    mutate(PCNT = PF/PA) %>%
    mutate(RV = PTS + PCNT) %>%
    arrange(desc(RV)) %>%
    mutate(rank = rank(-RV)) %>%
    mutate(simulation = sim)
  
  
  results <- rbind(results, tmp)
  if(sim %in% round(seq(0, noOfSims, noOfSims/100))){
    print(paste0(round(sim/noOfSims*100),'% complete'))
  }
}

summary <- results %>%
  mutate(C = ifelse(rank == 1, 1,0)) %>%
  mutate(RU = ifelse(rank == 2, 1,0)) %>%
  mutate(GF = ifelse(rank <= 2, 1, 0)) %>%
  group_by(team) %>%
  summarise(
    CHAMPION = sum(C),
    G_FINALIST = sum(GF),
    SIM = n_distinct(simulation)
  ) %>%
  arrange(desc(CHAMPION), desc(G_FINALIST)) %>%
  mutate(CHAMP_PCNT = round(CHAMPION/sim*100)) %>%
  mutate(GF_PCNT = round(G_FINALIST/sim*100))

summary[!is.na(summary$team),]

