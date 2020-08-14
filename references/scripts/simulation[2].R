options(stringsAsFactors = FALSE)

library(tidyverse)
library(xlsx)
library(tictoc)

setwd('\\\\aur.national.com.au/User_Data/AU-VIC-DOCKLANDS-3DOCK-03/UserData/p731535/Fantasy Banter/2019/Simulation')

tic()

raw.data <- as_tibble(read.xlsx('../Master Fantasy Banter Workbook.xlsx', sheetName= 'DATA - Fixture') %>%
                         filter(SEASON == 2019) %>%
                         filter(!is.na(TEAM.NAME)))

# raw.data <- as_tibble(read.csv('../Master Fantasy Banter Workbook.csv') %>%
#                           filter(SEASON == 2019) %>%
#                           filter(!TEAM.NAME=="#N/A"))


# BYE WEIGHTS
# raw.data$TEAM.SCORE[raw.data$ROUND %in% c(12,13,14) & raw.data$COACH == 'CHIEF'] <- c(1803,1404,1241)
# raw.data$TEAM.SCORE[raw.data$ROUND %in% c(12,13,14) & raw.data$COACH == 'GARTER'] <- c(1615,1470,1541)
# raw.data$TEAM.SCORE[raw.data$ROUND %in% c(12,13,14) & raw.data$COACH == 'JMERC'] <- c(1514,1595,1672)
# raw.data$TEAM.SCORE[raw.data$ROUND %in% c(12,13,14) & raw.data$COACH == 'LESTER'] <- c(1772,1624,1536)
# raw.data$TEAM.SCORE[raw.data$ROUND %in% c(12,13,14) & raw.data$COACH == 'MELONS'] <- c(1690,1656,1440)
# raw.data$TEAM.SCORE[raw.data$ROUND %in% c(12,13,14) & raw.data$COACH == 'KAPPAZ'] <- c(1669,1569,1758)
# raw.data$TEAM.SCORE[raw.data$ROUND %in% c(12,13,14) & raw.data$COACH == 'PMAC'] <- c(1458,1794,1546)
# raw.data$TEAM.SCORE[raw.data$ROUND %in% c(12,13,14) & raw.data$COACH == 'RICHO'] <- c(1551,1553,1720)


list.coach <- unique(raw.data$COACH)
results <- tibble()

noOfSims <- 10000

for( sim in 1:noOfSims){
  data <- raw.data
  for (i in 1:length(list.coach)){
    
    scores <- data$TEAM.SCORE[data$COACH == list.coach[i]]
    scores <- scores[scores>0]
    
    for (j in (length(scores)+1):21){
      mean <- mean(scores)
      sd <- sd(scores)
      scores[j] <- round(rnorm(1, mean, sd),0)
    }
    
    data$TEAM.SCORE[data$COACH == list.coach[i]] <- scores
    data$OPPONENT.SCORE[data$OPPONENT.COACH == list.coach[i]] <- scores
  }
  
  rd <- 14
  
  tmp <- data %>%
    mutate(DIFFERENTIAL = ifelse(ROUND<=rd,DIFFERENTIAL, TEAM.SCORE - OPPONENT.SCORE)) %>%
    mutate(WIN = ifelse(DIFFERENTIAL>0, 1, 0)) %>%
    mutate(DRAW = ifelse(DIFFERENTIAL==0, 1, 0)) %>%
    mutate(LOSS = ifelse(DIFFERENTIAL<0, 1, 0)) %>%
    group_by(COACH) %>%
    summarise(
      W = sum(WIN),
      D = sum(DRAW),
      L = sum(LOSS),
      PF = sum(TEAM.SCORE),
      PA = sum(OPPONENT.SCORE)
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
  group_by(COACH) %>%
  summarise(
    RANK.MEAN = mean(rank),
    WIN.MAX = max(W),
    WIN.MIN = min(W),
    FINALS = sum(TOP4),
    sim = max(simulation)
  ) %>%
  arrange(RANK.MEAN) %>%
  mutate(PCNT = round(FINALS/sim*100))

toc()

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
  filter(COACH=='PMAC' & W <= 11)%>%
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












