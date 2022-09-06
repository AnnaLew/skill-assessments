library(glue)
suppressMessages(library(dplyr))

# DEFINING FUNCTIONS

# Choosing the sign by the user
x_or_o <- function() {
  cat("X or O? ")
  sign_choice <- toupper(readLines(con = con, n = 1))
  return(sign_choice)
}

choose_player_sign <- function() {
  user_sign <- x_or_o()
  while (!(user_sign %in% c("X", "O"))) {
    print("Wrong input!")
    user_sign <- x_or_o()
  }
  return(user_sign)
}

# Defining the game sign
define_game_sign <- function(user_sign) {
  if (user_sign == "X") {
    game_sign <- "O"
  } else {
    game_sign <- "X"
  }
  return(game_sign)
}

# Printing the round number
print_round_number <- function(x) {
  round_text <- glue("\n\n########################
####### Round #{round_number} #######
########################")
  cat(round_text)
  x <- x + 1
  return(x)
}

# Board setup
board_setup <- function() {
  board <- matrix(NA, nrow = 3, ncol = 3)
  return(board)
}

# Displaying the board
current_board_display <- function() {
  cat("\n\nCurrent board:\n~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  print(board)
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~\n")
}

# Choosing the row and column by the player
is_correct_number <- function(num) {
  if (between(num, 1, 3)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

questions_player_move <- function(player_sign) {
  cat(glue("\nPlayer {player_sign} turn:"))
  cat("\n\nWhat row? ")
  row_number <- as.integer(readLines(con = con, n = 1))
  while (is.na(row_number) || !(is_correct_number(row_number))) {
    cat("Enter row number again:")
    row_number <- as.integer(readLines(con = con, n = 1))
  }
  cat("What column? ")
  column_number <- as.integer(readLines(con = con, n = 1))
  while (!(is_correct_number(column_number))) {
    cat("Enter column number again:")
    column_number <- as.integer(readLines(con = con, n = 1))
  }
  cat(glue("Place {player_sign} at row {row_number}, column {column_number}? [y/n] "))
  is_correct <- tolower(readLines(con = con, n = 1))
  move_data <- list("row" = row_number, "column" = column_number, "correct" = is_correct)
  return(move_data)
}

player_move <- function(board) {
  move_data <- questions_player_move(player_sign)
  Sys.sleep(1)
  while (move_data$correct != "y" || !is.na(board[move_data$row, move_data$column])) {
    print("This place is occupied, you have to choose again.")
    move_data <- questions_player_move(player_sign)
  }
  board[move_data$row, move_data$column] <- player_sign
  print("Move placed!")
  return(board)
}

# Game turn
generate_game_turn <- function() {
  move <- FALSE
  cat(glue("\nPlayer {game_sign} turn:"))
  while (!move) {
    rand_row <- sample(1:3, 1)
    rand_col <- sample(1:3, 1)
    if (is.na(board[rand_row, rand_col])) {
      board[rand_row, rand_col] <- game_sign
      move <- TRUE
    } else {
      move <- FALSE
    }
  }
  cat("\n\nComputer move registered")
  return(board)
}

# Game structure
gameplay <- function() {
  if (player_sign == "X") {
    board <- player_move(board)
    # board <- generate_game_turn()
  } else {
    board <- generate_game_turn()
    # board <- player_move(board)
  }
  print(board)
  return(board)
}

# Checking if anyone won
game_over <- function(board) {
  for (i in 1:ncol(board)) {
    if (!(NA %in% board[, i])) {
      if (length(unique(board[, i])) == 1) {
        if (unique(board[, i]) == player_sign) {
          print("You won!")
          return(FALSE)
        } else {
          print("You lost!")
          return(FALSE)
        }
      }
    } else if (!(NA %in% board[i, ])) {
      if (board[i, 1] == board[i, 2] && board[i, 2] == board[i, 3]) {
        if (board[i, ][1] == player_sign) {
          print("You won!")
          return(FALSE)
        } else {
          print("You lost!")
          return(FALSE)
        }
      }
    }
  }
  if (!is.na(board[2, 2])) {
    if (length(unique(c(board[1, 1], board[2, 2], board[3, 3]))) == 1 || length(unique(c(board[3, 1], board[2, 2], board[1, 3]))) == 1) {
      if (board[2, 2] == player_sign) {
        print("You won!")
        return(FALSE)
      } else if (board[2, 2] == game_sign) {
        print("You lost!")
        return(FALSE)
      }
    } else if (!any(is.na(board))) {
      print("Game over, no more space left!")
      return(FALSE)
    }
  }
  return(TRUE)
}


# STARTING THE GAME

if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}

player_sign <- choose_player_sign()

game_sign <- define_game_sign(player_sign)

round_number <- 1

board <- board_setup()

game_on <- TRUE

while (game_on) {
  round_number <- print_round_number(round_number)
  Sys.sleep(1)
  current_board_display()
  if (player_sign == "X") {
    Sys.sleep(1)
    board <- player_move(board)
    Sys.sleep(1)
    current_board_display()
    game_on <- game_over(board)
    if (!game_on) {
      break
    }
    Sys.sleep(1)
    board <- generate_game_turn()
  } else {
    board <- generate_game_turn()
    Sys.sleep(1)
    current_board_display()
    game_on <- game_over(board)
    if (!game_on) {
      break
    }
    Sys.sleep(1)
    board <- player_move(board)
  }
  current_board_display()
  game_on <- game_over(board)
}
