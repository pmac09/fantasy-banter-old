options(stringsAsFactors = FALSE)

library(shiny)
library(miniUI)
library(DT)
library(Jmisc)

sourceAll('./functions/')

ui <- miniPage(
  
  ## TITLE ----
  gadgetTitleBar(
    "ASL GRAND FINAL 2020"
  ),
  miniTabstripPanel(
    
    ## SCORING TAB ----
    miniTabPanel(
      "Live Scores", 
      icon = icon("clipboard"),
      miniContentPanel(
        fillCol( flex= c(NA,NA,NA,1),
          textOutput("gameTime"),
          textOutput("pointsAllocated"),
          textOutput("margin"),
          dataTableOutput("liveData")

        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  auth_headers <- get_sc_auth(cid, tkn)
  settings <- get_sc_settings(auth_headers)
  
  rnd <- settings$competition$next_round
  
  league_raw <- get_sc_league_raw(auth_headers, rnd)
  player_data <- get_sc_player_data(get_sc_player_raw(auth_headers, rnd))
  team_data <- get_sc_team_data(league_raw)
  
  tData <- left_join(team_data, player_data, by = c('player_id' = 'player_id')) %>%
    mutate(points = as.numeric(points)) %>%
    filter(picked == TRUE)
  
  
  ff_data_r <- reactive({
    invalidateLater(30 * 1000)
    ff_data <- ff_get_game_data(7454)
    return(ff_data)
  })
  
  game_time_r <- reactive({
    ff_data <- ff_data_r()
    game_time <- ff_data$gametime[1]
    return(game_time)
  })
  
  points_allocated_r <- reactive({
    ff_data <- ff_data_r()
    points_allocated <- sum(as.numeric(ff_data$supercoach)) 
    return(points_allocated)
  })
  
  margin_r <- reactive({
    live_team <- live_team_r()
    margin <- live_team$score[nrow(live_team)] - 1700
    return(margin)
  })

  live_team_r <- reactive({ 
    
    ff_live_data <- ff_data_r()
    
    tData <- left_join(team_data, player_data, by = c('player_id' = 'player_id')) %>%
      mutate(points = as.numeric(points)) %>%
      filter(picked == TRUE)
    
    lData <- ff_live_data[,c(
      'player.feed_id',
      'supercoach'
    )] %>%
      mutate(supercoach = as.numeric(supercoach)) %>%
      mutate(player.feed_id = as.numeric(player.feed_id)) %>%
      mutate(supercoach = ifelse(is.na(supercoach), 0, supercoach))
    
    projection_data <- left_join(tData, lData, by=c('feed_id' = 'player.feed_id')) %>%
      rowwise() %>%
      mutate(live = sum(points, supercoach, na.rm=T)) %>%
      ungroup()
    
    summary <- projection_data %>%
      mutate(player = ifelse(!is.na(supercoach), last_name, user_team_id)) %>%
      group_by(user_team_id, player) %>%
      summarise(
        score = sum(live),
        .groups = 'drop'
      ) %>%
      arrange(desc(score))
    
    
    totals <- summary %>%
      group_by(user_team_id) %>%
      summarise(
        score = sum(score),
        .groups = 'drop'
      )
    
    live_team <- bind_rows(
      summary %>% filter(user_team_id == 186),
      totals %>% filter(user_team_id == 186)
    )
    
    return(live_team)
    
  })
  
  
  output$liveData <- renderDataTable({

    data <- live_team_r()[,c(2:3)]
    
    table <- datatable(data,
                       rownames= FALSE,
                       options= list(paging= FALSE, 
                                     searching= FALSE, 
                                     ordering=FALSE, 
                                     info=FALSE,
                                     columnDefs = list(
                                       list(
                                         className = 'dt-center', 
                                         targets = 1:(ncol(data)-1)))
                       ),
                       extensions="Responsive"
    ) %>%
      formatStyle( 0, target= 'row', lineHeight='70%')
    
    return(table)
  })
  
  output$gameTime <- renderText({
    paste0("Game Time: ", game_time_r())
  })
  
  output$pointsAllocated <- renderText({
    paste0("Points Allocated: ", points_allocated_r())
  })
  
  output$margin <- renderText({
    paste0("Margin: ", margin_r())
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

