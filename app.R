library(shiny)
library(ggplot2)

# DEFINE USER INTERFACE ------------------------------------------------------#
ui <- fluidPage(
  
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e6e6fa; /* Light purple background */
      }
      h1 {
        color: #ff1493; /* Neon pink title */
        text-align: center;
        font-family: 'Arial', sans-serif;
        font-size: 48px;
        margin-bottom: 30px;
      }
      .title-container {
        display: flex;
        align-items: center;
        justify-content: center;
      }
      h3 {
        color: #8b0000; /* Dark red for player scores */
        font-family: 'Arial', sans-serif;
        margin: 10px 0;
      }
      h4 {
        color: #000080; /* Navy blue for turn indicator */
        font-family: 'Arial', sans-serif;
        font-size: 28px;
      }
      .well {
        background-color: #f5f5f5; /* Light grey background for input sections */
        border-radius: 10px;
      }
      .btn {
        background-color: #006400; /* Dark green buttons */
        color: white;
        font-weight: bold;
        margin: 10px 0;
      }
      .btn:hover {
        background-color: #228b22; /* Lighter green on hover */
      }
    "))
  ),
  
  # Application title with image next to it
  titlePanel(
    div(
      class = "title-container",
      h1("Shiny Darts!")
    )
  ),
  
  # FIRST TAB - MAIN GAME ---------------------------------------------------------
  tabsetPanel(
    tabPanel("Game",
             sidebarLayout(
               sidebarPanel(
                 numericInput("num_players", "Number of Players:", value = 2, min = 2, max = 10),
                 actionButton("start_game", "Start Game"),
                 hr(),
                 uiOutput("player_names_ui"),
                 hr(),
                 uiOutput("turn_indicator"),
                 numericInput("current_throw", "Enter current throw:", value = 0, min = 0),
                 actionButton("enter_score", "Enter")
               ),
               
               # Main panel to display player scores
               mainPanel(
                 uiOutput("score_displays"),
                 uiOutput("winner_message") # Display winner message
               )
             )
    ),
    
    # TAB 2: ANALYTICS TAB ------------------------------------------------------------
    tabPanel("Analytics",
             fluidRow(
               column(6, h3("Player")),
               column(3, h3("Number of Throws")),
               column(3, h3("Average Score"))
             ),
             hr(),
             uiOutput("analytics_table"),
             hr(),
             plotOutput("time_series_plot") # Add plot output for time series
    )
  )
)


# DEFINE SERVER LOGIC --------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive values to store the game state
  game_state <- reactiveValues(
    scores = NULL,
    current_turn = 1,
    num_players = 2,
    player_names = NULL,
    winner = NULL,  # store winner's name
    throw_counts = NULL,  # store number of throws per player
    throw_sums = NULL,  # store sum of all throws per player 
    time_series = vector("list", 10)  # Assuming a max of 10 players
  )
  
  # Generate input fields for player names
  output$player_names_ui <- renderUI({
    num_players <- input$num_players
    name_inputs <- lapply(1:num_players, function(i) {
      textInput(inputId = paste0("player_name_", i), label = paste("Enter name for Player", i), value = paste("Player", i))
    })
    do.call(tagList, name_inputs)
  })
  
  # Update number of players and initialize scores, names, and throw counters
  observeEvent(input$start_game, {
    game_state$num_players <- input$num_players
    game_state$scores <- rep(501, game_state$num_players)
    game_state$current_turn <- 1
    game_state$player_names <- sapply(1:game_state$num_players, function(i) {
      input[[paste0("player_name_", i)]]
    })
    game_state$winner <- NULL  # Reset winner when game starts
    game_state$throw_counts <- rep(0, game_state$num_players)  # Initialize throw counts
    game_state$throw_sums <- rep(0, game_state$num_players)  # Initialize throw sums
    game_state$time_series <- lapply(1:game_state$num_players, function(i) {
      data.frame(round = integer(), score = integer())
    })  # Initialize time series data
  })
  
  # Display scores for each player
  output$score_displays <- renderUI({
    if (is.null(game_state$scores)) return(NULL)
    
    score_ui <- lapply(1:game_state$num_players, function(i) {
      fluidRow(
        column(6, h3(paste(game_state$player_names[i], "Score:"))),
        column(6, h3(textOutput(paste0("score_", i))))
      )
    })
    do.call(tagList, score_ui)
  })
  
  # Update score output for each player
  observe({
    if (!is.null(game_state$scores)) {
      lapply(1:game_state$num_players, function(i) {
        output[[paste0("score_", i)]] <- renderText({
          game_state$scores[i]
        })
      })
    }
  })
  
  # Update turn indicator
  output$turn_indicator <- renderUI({
    if (!is.null(game_state$scores) && is.null(game_state$winner)) {
      h4(paste(game_state$player_names[game_state$current_turn], "'s turn"))
    }
  })
  
  # Display winner message
  output$winner_message <- renderUI({
    if (!is.null(game_state$winner)) {
      h1(paste(game_state$winner, "wins!"), style = "color: #ff1493; text-align: center;")
    }
  })
  
  # Update score for the current player and check for a winner
  observeEvent(input$enter_score, {
    if (!is.null(game_state$scores) && is.null(game_state$winner)) {
      current_score <- game_state$scores[game_state$current_turn]
      throw_value <- input$current_throw
      
      # Subtract current throw from the player's score
      new_score <- current_score - throw_value
      if (new_score >= 0) {
        game_state$scores[game_state$current_turn] <- new_score
        game_state$throw_counts[game_state$current_turn] <- game_state$throw_counts[game_state$current_turn] + 1
        game_state$throw_sums[game_state$current_turn] <- game_state$throw_sums[game_state$current_turn] + throw_value
        
        # Update time series data
        ts_data <- game_state$time_series[[game_state$current_turn]]
        new_round <- nrow(ts_data) + 1
        game_state$time_series[[game_state$current_turn]] <- rbind(ts_data, data.frame(round = new_round, score = new_score))
      }
      
      # Check if current player has won
      if (game_state$scores[game_state$current_turn] == 0) {
        game_state$winner <- game_state$player_names[game_state$current_turn]
      } else {
        # Move to next player's turn
        game_state$current_turn <- ifelse(game_state$current_turn %% game_state$num_players == 0, 1, game_state$current_turn + 1)
      }
    }
  })
  
  # Generate analytics table
  output$analytics_table <- renderUI({
    if (is.null(game_state$scores)) return(NULL)
    
    analytics_ui <- lapply(1:game_state$num_players, function(i) {
      avg_score <- if (game_state$throw_counts[i] > 0) {
        round(game_state$throw_sums[i] / game_state$throw_counts[i], 2)
      } else {
        0
      }
      fluidRow(
        column(6, h4(game_state$player_names[i])),
        column(3, h4(game_state$throw_counts[i])),
        column(3, h4(avg_score))
      )
    })
    do.call(tagList, analytics_ui)
  })
  
  # Render time series plot
  output$time_series_plot <- renderPlot({
    if (is.null(game_state$time_series) || length(game_state$time_series) == 0) return(NULL)
    
    ts_data_list <- game_state$time_series
    plot_list <- lapply(1:game_state$num_players, function(i) {
      ts_data <- ts_data_list[[i]]
      ggplot(ts_data, aes(x = round, y = score)) +
        geom_line() +
        geom_point() +
        ggtitle(paste("Player", i, "-", game_state$player_names[i])) +
        labs(x = "Round", y = "Score") +
        theme_minimal()
    })
    
    # Combine plots into a single plot grid
    do.call(gridExtra::grid.arrange, c(plot_list, ncol = 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
