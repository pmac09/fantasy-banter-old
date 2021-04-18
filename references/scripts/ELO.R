
options(stringsAsFactors = FALSE)
library(tidyverse)

elo <- read.csv('./references/data/clean/Pong_stats.csv') %>%
  select(YEAR, GAME, TEAM, PLAYER, HITS, OVERTHROWS)


elo2 <- as_tibble(elo) %>%
  rowwise() %>%
  mutate(SCORE = max(HITS - OVERTHROWS, 0)) %>%
  mutate(GAME_ID = paste0(YEAR,"_",GAME)) %>%
  mutate(RATING = 1000) %>%
  select(GAME_ID, TEAM, PLAYER, RATING, SCORE) %>%
  mutate(NEW_RATING = NA) 

#%>% 
#  filter(!PLAYER %in% c('MELONS','GARTER'))


games <- unique(elo2$GAME_ID)

h2h<- tibble()

for (i in 1:length(games)){

game <- games[i]

game_data <- elo2[elo2$GAME_ID == game, c('GAME_ID', 'TEAM', 'PLAYER', 'RATING', 'SCORE', 'NEW_RATING')]

game_data2 <- left_join(game_data, game_data, by=c('GAME_ID')) %>%
  filter(PLAYER.x != PLAYER.y) %>%
  filter(TEAM.x != TEAM.y) %>%
  mutate(K = 20) %>%
  mutate(ACTUAL = ifelse(SCORE.x > SCORE.y, 1, ifelse(SCORE.x == SCORE.y, 0.5, 0))) %>%
  mutate(EXPECTED = 1/(1+10^((RATING.y-RATING.x)/400))) %>%
  mutate(Q = 2.2/((ifelse(ACTUAL == 1, SCORE.x, SCORE.y) - ifelse(ACTUAL == 1, SCORE.y, SCORE.x))*0.001+2.2)) %>%
  mutate(MoVM = log(abs(SCORE.x - SCORE.y)+1)*Q) %>%
  mutate(change = K * (ACTUAL - EXPECTED) * MoVM) %>%
  mutate(change = round(change,0))

h2h <-bind_rows(h2h, game_data2[,c('GAME_ID','PLAYER.x', 'PLAYER.y', 'change')])

game_data3 <- game_data2 %>%
  group_by(PLAYER.x) %>%
  summarise(
    CHANGE = sum(change),
    .groups = 'drop'
  )

game_data4 <- left_join(game_data, game_data3, by=c('PLAYER' = 'PLAYER.x')) %>%
  mutate(NEW_RATING = RATING + CHANGE) %>%
  select(GAME_ID, PLAYER, NEW_RATING)


elo2 <- left_join(elo2, game_data4, by=c('GAME_ID', 'PLAYER')) %>%
  mutate(NEW_RATING=coalesce(NEW_RATING.x,NEW_RATING.y)) %>%
  select(-NEW_RATING.x,-NEW_RATING.y) %>%
  ungroup()


idx <- max(grep(game, elo2$GAME_ID)) 

if(idx < nrow(elo2)){
  elo2 <- left_join(elo2, game_data4[,c('PLAYER','NEW_RATING')], by=c('PLAYER'))%>%
    mutate(rownum = row_number()) %>%
    mutate(RATING = ifelse(rownum > idx & !is.na(NEW_RATING.y), NEW_RATING.y, RATING)) %>%
    select(-NEW_RATING.y,-rownum) %>%
    rename(NEW_RATING = NEW_RATING.x)
}

}

write.csv(elo2,'./references/data/clean/Pong_ELO.csv')


h2h_summary <- h2h %>%
  group_by(PLAYER.x, PLAYER.y) %>%
  summarise(
    total = sum(change)
  ) %>%
  arrange(desc(total))

print(h2h_summary, n=20)

h2h_summary %>%
  filter(PLAYER.x == 'PMAC')

h2h %>%
  filter(PLAYER.x == 'RICHO') %>%
  filter(PLAYER.y == 'KAPPAZ')

