
options(stringsAsFactors = FALSE)
library(shiny)
library(httr)
library(tidyverse)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #--------------------------------------- 
  # SETUP
  source('./functions/secrets.R')
  source('./functions/get_auth.R')
  source('./functions/get_supercoach_data.R')
  source('./functions/get_fixture_data.R')
  source('./functions/get_sc_teams.R')
  source('./functions/get_selected_game.R')
  source('./functions/get_selected_team.R')
  
  
  #--------------------------------------
  # ON START UP
  
  # Get authentication 
  auth_headers <- get_auth(cid, tkn)
  
  # Get game settings
  settings <- content(GET(
    url = paste0('https://supercoach.heraldsun.com.au/2020/api/afl/draft/v1/settings'),
    config = auth_headers
  ))
  

  
  
  
  #--------------------------------------
  # Reactive Functions
  sc_data <- reactive({get_supercoach_data(auth_headers, settings$competition$next_round)})
  fixture_data <- reactive({get_fixture_data(sc_data())})
  sc_teams <- reactive({get_sc_teams(sc_data())})
  selected_game <- reactive({get_selected_game(fixture_data(), input$lstMatchup)})
  
  
  #--------------------------------------
  # UI RENDERS
  output$uiMatchups <- renderUI({
    
    matchups <- fixture_data()$matchup
    
    ui <-  selectInput('lstMatchup', h3("Match-up:"), 
                       choices = matchups, 
                       selected = 1)
    
    return(ui)
  })

  output$tblTeam1 <- renderDataTable({
    req(input$lstMatchup)
    
    team <- get_selected_team(sc_teams(), selected_game(), TRUE)
    
    return(team)
  })
  
  output$uiTeam2 <- renderUI({
    ui <- teamPanel()
    return(ui)
  })
  
  #--------------------------------------
  # functions
  
  
  teamPanel <- function(team_data=NULL){
    ui <- column(6, h3("Team 2"))
    return(ui)
  }


  
  
})
