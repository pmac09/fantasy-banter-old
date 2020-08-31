source('./references/functions/load.R')

# Get authentication 
auth_headers <- get_sc_auth(cid, tkn)
settings <- get_sc_settings(auth_headers)

rnd <- settings$competition$next_round

league_raw <- get_sc_league_raw(auth_headers, rnd)
player_data <- get_sc_player_data(get_sc_player_raw(auth_headers, rnd))
team_data <- get_sc_team_data(league_raw)
fixture_data <- get_sc_fixture_data(league_raw)

# Today's games via fanfooty
ff_fixture <- ff_get_fixture_data(live = TRUE) 

# Current Game
ff_live_data <- ff_get_game_data(ff_fixture$game_id[2])

tData <- left_join(team_data, player_data, by = c('player_id' = 'player_id')) %>%
  mutate(points = as.numeric(points)) %>%
  filter(picked == TRUE)

lData <- ff_live_data[,c(
  'player.feed_id',
  'supercoach'
)] %>%
  mutate(supercoach = as.numeric(supercoach)) %>%
  mutate(player.feed_id = as.numeric(player.feed_id))

projection_data <- left_join(tData, lData, by=c('feed_id' = 'player.feed_id')) %>%
  rowwise() %>%
  mutate(live = sum(points, supercoach, na.rm=T)) %>%
  ungroup()


teams1 <- fixture_data[1,c(
  'team_id',
  'team'
)]
teams2 <- fixture_data[1,c(
  'opponent_team_id',
  'opponent_team'
)]

names(teams2) <- names(teams1)
teams <- bind_rows(teams1,teams2)

projection_data <- left_join(projection_data, teams, by=c('user_team_id'='team_id'))


summary <- projection_data %>%
  mutate(player = ifelse(!is.na(supercoach), last_name, team)) %>%
  group_by(user_team_id, player) %>%
  summarise(
    score = sum(live)
  ) %>%
  arrange(desc(score))


totals <- summary %>%
  group_by(user_team_id) %>%
  summarise(
    score = sum(score)
  )

bind_rows(
  summary %>% filter(user_team_id == 182),
  totals %>% filter(user_team_id == 182)
)

bind_rows(
  summary %>% filter(user_team_id == 5524),
  totals %>% filter(user_team_id == 5524)
)


bind_rows(
  summary %>% filter(user_team_id == 9806),
  totals %>% filter(user_team_id == 9806)
)

bind_rows(
  summary %>% filter(user_team_id == 9867),
  totals %>% filter(user_team_id == 9867)
)


sum(lData$supercoach)

