
get_sc_settings <- function(auth_headers){
  
  settings <- content(GET(
    url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/settings'),
    config = auth_headers
  ))
  
  return(settings)
}


