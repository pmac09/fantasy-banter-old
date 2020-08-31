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

byes <- list(
  'R14' = c('ADE','BRL'),
  'R15' = c('GCS','NTH','PTA','WBD','GEE','STK'),
  'R16' = c('COL','RIC')
)


## GET FIXTURE ------------------

# fixture <- tibble() 
# for(i in 1:16){
#   
#   league_raw <- get_sc_league_raw(auth_headers, i)
#   fixture_data <- get_sc_fixture_data(league_raw)
#   
#   fixture <- bind_rows(fixture, fixture_data)
# }

fixture <- read_csv('./references/data/clean/2020_Fixture_Data.csv')

fixture <- left_join(fixture, team_data, by=c('round','team_id'='user_team_id')) %>%
  rename(home_points = points)

fixture <- left_join(fixture, team_data, by=c('round','opponent_team_id'='user_team_id')) %>%
  rename(away_points = points)

fixture <- fixture %>%
  mutate(round = as.numeric(round)) %>%
  mutate(fixture = as.numeric(fixture)) %>%
  arrange(round, fixture)


## START SIM ------------------

list.coach <- unique(fixture$coach)

noOfSims <- 10000

results <- tibble()
for( sim in 1:noOfSims){
  
  data <- fixture[1:9]
  
  ## Simulate team
  sim_teams <- team_data
  for(i in rnd:16){
    
    proj <- live_teams %>%
      rowwise() %>%
      mutate(points = round(rnorm(1, mean, sd),0)) %>%
      mutate(points = ifelse(points<0, min, points))
    
    bye <- paste0('R',i)
    
    if(bye %in% names(byes)){
      proj$points[proj$team_abbrev %in% byes[[bye]]] <- 0
    }
    
    proj_teams <- proj %>%
      group_by(user_team_id) %>%
      summarise(
        points = sum(points),
        .groups = 'drop'
      ) %>%
      mutate(round = i)
    
    sim_teams <- bind_rows(sim_teams, proj_teams)
    
  }
  
  names(sim_teams)[3] <- 'home_points'
  data <- left_join(data, sim_teams, by=c('round', 'team_id'='user_team_id'))
  
  names(sim_teams)[3] <- 'away_points'
  data <- left_join(data, sim_teams, by=c('round', 'opponent_team_id'='user_team_id'))
  

  tmp <- data %>%
    mutate(DIFFERENTIAL = home_points - away_points) %>%
    mutate(WIN = ifelse(DIFFERENTIAL>0, 1, 0)) %>%
    mutate(DRAW = ifelse(DIFFERENTIAL==0, 1, 0)) %>%
    mutate(LOSS = ifelse(DIFFERENTIAL<0, 1, 0)) 
  
  # t <- data %>%
  #   filter(round == 15) %>%
  #   filter(team %in% c('Coronaviney', "HoweIMetYourMother")) %>%
  #   filter(home_points > away_points)
  # 
  # if(nrow(t) < 2) next
  
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
  mutate(TOP4 = ifelse(rank <= 4, 1,0)) %>%
  group_by(team) %>%
  summarise(
    RANK.MEAN = mean(rank),
    WIN.MAX = max(W),
    WIN.MIN = min(W),
    FINALS = sum(TOP4),
    sim = n_distinct(simulation)
  ) %>%
  arrange(RANK.MEAN) %>%
  mutate(PCNT = round(FINALS/sim*100))



summary


bkp <- results



smy <- results %>%
  group_by(team) %>%
  summarise(
    p1 = sum(ifelse(rank == 1, 1,0)),
    p2 = sum(ifelse(rank == 2, 1,0)),
    p3 = sum(ifelse(rank == 3, 1,0)),
    p4 = sum(ifelse(rank == 4, 1,0)),
    p5 = sum(ifelse(rank == 5, 1,0)),
    p6 = sum(ifelse(rank == 6, 1,0)),
    p7 = sum(ifelse(rank == 7, 1,0)),
    p8 = sum(ifelse(rank == 8, 1,0)),
    mean = mean(rank)
  ) %>%
  arrange(mean)

smy


pos_smy <- results %>%
  group_by(rank) %>%
  summarise(
    W_MAX = max(W),
    W_MED = median(W),
    W_MIN = min(W)
  ) %>%
  arrange(rank)

pos_smy

results %>%
  filter(team == 'Coronaviney' & rank == 6)

results %>%
  filter(simulation == 2689)


%>%
  group_by(rank)%>%
  summarise(
    count=n()
  ) %>%
  ungroup() %>%
  mutate(pcnt= count/sum(count))


results %>%
  filter(team == "What's Up, Doch?" & rank == 5)


results %>%
  filter(simulation == 8740)

  # smy <- results %>%
#   group_by(COACH) %>%
#   summarise(
#     p1 = round(sum(ifelse(rank == 1, 1,0))/noOfSims,3),
#     p2 = round(sum(ifelse(rank == 2, 1,0))/noOfSims,3),
#     p3 = round(sum(ifelse(rank == 3, 1,0))/noOfSims,3),
#     p4 = round(sum(ifelse(rank == 4, 1,0))/noOfSims,3),
#     p5 = round(sum(ifelse(rank == 5, 1,0))/noOfSims,3),
#     p6 = round(sum(ifelse(rank == 6, 1,0))/noOfSims,3),
#     p7 = round(sum(ifelse(rank == 7, 1,0))/noOfSims,3),
#     p8 = round(sum(ifelse(rank == 8, 1,0))/noOfSims,3),
#     mean = mean(rank)
#   ) %>%
#   arrange(mean)












