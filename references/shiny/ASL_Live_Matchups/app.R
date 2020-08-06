options(stringsAsFactors = FALSE)

library(shiny)
library(tidyverse)
library(httr)
library(data.table)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ASL Live Matchups"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      uiOutput('uiSelectMatchup')
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("text")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## FUNCTIONS ------------------------------------
  source('./functions/secrets.R')
  source('./functions/get_auth.R')
  
  source('./functions/sc_get_league_data.R')
  source('./functions/sc_get_fixture_data.R')
  source('./functions/sc_get_team_data.R')  
  
  source('./functions/ff_get_fixture.R')  
  source('./functions/ff_get_game_data.R')
  
  
  ## SETUP ------------------------------------
  
  # Get authentication 
  auth_headers <- get_auth(cid, tkn)
  
  # Get game settings
  settings <- content(GET(
    url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/settings'),
    config = auth_headers
  ))
  
  # Get current round
  rnd <- settings$competition$next_round
  
  
  
  ## REACTIVE FUNCTIONS ------------------------------------
  league_data <- reactive({
    sc_get_league_data(auth_headers, rnd)
  }) # League data for current round
  team_data <- reactive({
    sc_get_team_data(league_data())
  }) # Teams for the selected round
  fixture_data <- reactive({
    sc_get_fixture_data(league_data())
  }) # Fixture for the selected round
  selected_fixture <- reactive({
    req(input$lstSelectMatchup)
    selection <- fixture_data() %>%
      filter(matchup == input$lstSelectMatchup)
    
    return(selection)
    
  }) # Selected game from fixture
  
  ff_fixture <- reactive({
    ff_get_fixture(live = TRUE)
  }) # Today's games via fanfooty
  
  ff_live_data <- function(ff_fixture){
    live_data <- tibble()
    for(i in ff_fixture$game_id){
      game_data <- ff_get_game_data(i)
      live_data <- bind_rows(live_data, game_data)
    }
    return(live_data)
  }
  ff_live_data_r <- reactive({
    ff_live_data(ff_fixture())
  }) # Live scores
  
  projection_data <- function(team_data, ff_live_data){
    
    tData <- team_data[,c(
      'user_team_id',
      'player.feed_id',
      'player.first_name',
      'player.last_name',
      'player.played_status.status',
      'points'
    )]
    
    lData <- ff_live_data[,c(
      'player.feed_id',
      'supercoach'
    )]
    
    projection_data <- left_join(tData, lData, by=c('player.feed_id'))
  }
  projection_data_r <- reactive({
    projection_data(team_data(), ff_live_data_r())
  })
  


  
  ## UI OUTPUT ------------------------------------
  output$uiSelectMatchup <- renderUI({
    
    fixture <- fixture_data()
    
    ui <-  selectInput('lstSelectMatchup', 
                       h3("Match-up:"), 
                       choices = fixture$matchup, 
                       selected = 1)
    return(ui)
  })
  
  output$text <- renderTable({
    head(projection_data_r())
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

