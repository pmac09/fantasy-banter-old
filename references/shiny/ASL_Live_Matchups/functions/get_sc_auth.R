
get_sc_auth <- function(cid, tkn){
  
  # POST request to get an access token
  auth <- content(POST(
    url = 'https://supercoach.heraldsun.com.au/2020/api/afl/classic/v1/access_token',
    body = list(
      grant_type = 'social',
      client_id = cid,
      client_secret = '',
      service = 'auth0',
      token = tkn
    ),
    encode = 'json'
  ))
  
  headers <- add_headers(
    Authorization = paste0('Bearer ', auth$access_token)
  )
  
  return(headers)
}