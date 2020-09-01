# Load supercoach functions
source('./references/functions/load.R')

auth_headers <- get_sc_auth(cid, tkn)
settings <- get_sc_settings(auth_headers)
rnd <- settings$competition$current_round

url <- paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/leagues/96/playersStatus')


loop <- TRUE
while(loop){
  
  data <- content(GET(url = url, config = auth_headers))
  data <- lapply(data, unlist)
  data <- bind_rows(lapply(data, as.data.frame.list))
  
  t <- data %>%
    group_by(trade_status) %>%
    summarise(
      n = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(n))
  
  msg <- paste0(Sys.time(), ': ', t$trade_status[1], '(', t$n[1],'); ', 
                t$trade_status[2], '(', t$n[2],'); ', 
                t$trade_status[3], '(', t$n[3],') ')
  message(msg)
  
  Sys.sleep(30)
  
  if(t$trade_status[1] == 'free_agent'){
    loop <- FALSE
  }
  
}




