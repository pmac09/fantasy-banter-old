# Load supercoach functions
source('./references/functions/load.R')

master_data <- read_csv('./references/data/clean/2020_Player_Data.csv') %>%
  mutate(round = as.character(round)) %>%
  mutate(user_team_id = as.character(user_team_id))

team_scores <- master_data %>%
  filter(picked == TRUE) %>%
  group_by(round, user_team_id) %>%
  summarise(points = sum(points, na.rm=T)) 

auth_headers <- get_sc_auth(cid, tkn)

fixture <- tibble() 
for(i in 1:16){
  
  league_raw <- get_sc_league_raw(auth_headers, i)
  fixture_data <- get_sc_fixture_data(league_raw)
  
  fixture <- bind_rows(fixture, fixture_data)
}

away <- fixture[,c(1,2,6,7,8,3,4,5,9)]
colnames(away) <- colnames(fixture)

fixture <- bind_rows(fixture, away) 

fixture <- left_join(fixture, team_scores, by=c('round','user_team1.id'='user_team_id')) %>%
  rename(home_points = points)

fixture <- left_join(fixture, team_scores, by=c('round','user_team2.id'='user_team_id')) %>%
  rename(away_points = points)

fixture <- fixture %>%
  mutate(round = as.numeric(round)) %>%
  mutate(fixture = as.numeric(fixture)) %>%
  arrange(round, fixture)


list.coach <- unique(fixture$user_team1.teamname)


noOfSims <- 10000

results <- tibble()
for( sim in 1:noOfSims){
  
  data <- fixture
  
  for (i in 1:length(list.coach)){
    
    scores <- data$home_points[data$user_team1.teamname == list.coach[i]]
    scores <- scores[!is.na(scores)]
    
    for (j in (length(scores)+1):16){
      mean <- mean(scores)
      sd <- sd(scores)
      scores[j] <- round(rnorm(1, mean, sd),0)
    }
    
    data$home_points[data$user_team1.teamname == list.coach[i]] <- scores
    data$away_points[data$user_team2.teamname == list.coach[i]] <- scores
  }
  
  
  tmp <- data %>%
    mutate(DIFFERENTIAL = home_points - away_points) %>%
    mutate(WIN = ifelse(DIFFERENTIAL>0, 1, 0)) %>%
    mutate(DRAW = ifelse(DIFFERENTIAL==0, 1, 0)) %>%
    mutate(LOSS = ifelse(DIFFERENTIAL<0, 1, 0)) %>%
    group_by(user_team1.teamname) %>%
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
  group_by(user_team1.teamname) %>%
  summarise(
    RANK.MEAN = mean(rank),
    WIN.MAX = max(W),
    WIN.MIN = min(W),
    FINALS = sum(TOP4),
    sim = max(simulation)
  ) %>%
  arrange(RANK.MEAN) %>%
  mutate(PCNT = round(FINALS/sim*100))



summary


bkp <- results



smy <- results %>%
  group_by(COACH) %>%
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
  filter(user_team1.teamname=='Bad Boys For Fyfe' & W == 3)


%>%
  group_by(rank)%>%
  summarise(
    count=n()
  ) %>%
  ungroup() %>%
  mutate(pcnt= count/sum(count))


results %>%
  filter(COACH == 'RICHO' & rank == 4)


results %>%
  filter(simulation == 7087)

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












