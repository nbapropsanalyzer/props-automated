
## NBA Box Scores "Scraper"
## December 2022
## rha

## Load Libraries & SetWD
library(hoopR)
library(rvest)
team_abbreviations <- read.csv("TeamNamesFull.csv", stringsAsFactors = FALSE)
away_team_abbreviations <- team_abbreviations
home_team_abbreviations <- team_abbreviations
colnames(away_team_abbreviations) <- c("Away", "Abbreviation")
colnames(home_team_abbreviations) <- c("Home", "Abbreviation")
away_hoopR_team_abbreviations <- read.csv("TeamNamesToHoopR.csv", stringsAsFactors = FALSE)
home_hoopR_team_abbreviations <- read.csv("TeamNamesToHoopR.csv", stringsAsFactors = FALSE)
colnames(away_hoopR_team_abbreviations) <- c("AwayTeam", "Away")
colnames(home_hoopR_team_abbreviations) <- c("HomeTeam", "Home")


## Find which games are being played today from BR
# Find today's date & month
Sys.setenv(TZ = "EST")
today <- Sys.Date()
today_month <- tolower(months(today))
today_year <- 2023
# Get today's games from site into data frame form
br_url <- paste0("https://www.basketball-reference.com/leagues/NBA_", today_year, "_games-",today_month,".html")
html <- xml2::read_html(br_url)
schedule_node <- rvest::html_node(html, "table#schedule")
schedule_table <- rvest::html_table(schedule_node, header = TRUE)
schedule_table$Date <- as.Date(schedule_table$Date, format = "%a, %b %d, %Y")
schedule_table <- schedule_table[which(schedule_table$Date == today), ]
colnames(schedule_table) <- c("Date", "Start (ET)", "Away", "AWPTS", "Home", "HMPTS", "Box", "Blank", "Attend.", "Notes")
away_teams <- join(schedule_table, away_team_abbreviations, by=c("Away"))$Abbreviation
home_teams <- join(schedule_table, home_team_abbreviations, by=c("Home"))$Abbreviation
todays_games <- cbind(away_teams, home_teams)
colnames(todays_games) <- c("Away", "Home")
todays_games <- as.data.frame(todays_games)
todays_games$matchup <- paste(todays_games$Away, "-", todays_games$Home)

final_table <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(final_table) <- c("Away", "Home", "Matchup", "Team", "Player")

## Find the players on each team
# Get the team roster - away
for(i in 1:nrow(todays_games)){
  away_team <- todays_games$Away[i]
  
  away_roster_url <- paste0("https://www.basketball-reference.com/teams/", away_team, "/2023.html")
  
  # Get the roster table
  away_all_html_tables <- xml2::read_html(away_roster_url) %>% html_table(fill=TRUE)
  
  # Get the players and clean the data
  away_roster_table <- away_all_html_tables[[1]]
  away_current_roster <- away_roster_table$Player
  # Remove players with "(TW)"
  away_current_roster_2 <- away_current_roster[-grep("TW", away_current_roster)]
  
  # Expand out the row for all players
  final_table <- rbind(final_table, cbind(do.call(rbind, replicate(length(away_current_roster_2), todays_games[i,], simplify=FALSE)), Team=away_team, Player=away_current_roster_2))
  
  home_team <- todays_games$Home[i]
  
  home_roster_url <- paste0("https://www.basketball-reference.com/teams/", home_team, "/2023.html")
  
  # Get the roster table
  home_all_html_tables <- xml2::read_html(home_roster_url) %>% html_table(fill=TRUE)
  
  # Get the players and clean the data
  home_roster_table <- home_all_html_tables[[1]]
  home_current_roster <- home_roster_table$Player
  # Remove players with "(TW)"
  home_current_roster_2 <- home_current_roster[-grep("TW", home_current_roster)]
  
  # Expand out the row for all players
  final_table <- rbind(final_table, cbind(do.call(rbind, replicate(length(home_current_roster_2), todays_games[i,], simplify=FALSE)), Team=home_team, Player=home_current_roster_2))
}

# Remove accents from the players
final_table$Player <- stri_trans_general(final_table$Player, "Latin-ASCII")

final_table$Opponent <- NA

temp_colnames_player <- c("name", "position", "min", "pts", "fg", "fg3", "reb", "ast", "game_date", "matchup")
# Create the necessary columns to hold the game played data
for(j in 1:20){
  final_table[, paste0("regular_", j, "_", temp_colnames_player)] <- NA
}
for(j in 1:5){
  final_table[i, paste0("opponent_", j, "_", temp_colnames_player)]  <- NA
}


## Get the boxscores using the hoopR package
box_scores <- rbind(load_nba_player_box(2023), load_nba_player_box(2022), load_nba_player_box(2021), load_nba_player_box(2020))

# Sort the data by game date
box_scores <- box_scores[order(-box_scores$game_date),] 

## For each of the players in the dataframe, get the game data and add it to the data frame
for(i in 1:nrow(final_table)){
  # Given a player, get the full player subset
  full_player_subset_of_data <- box_scores[which(box_scores$athlete_display_name == final_table$Player[i]),]
  
  # If there's no match, do a fuzzy match
  if(nrow(full_player_subset_of_data) == 0){
    print(paste0("going to a fuzzy match for: ", final_table$Player[i]))
    full_player_subset_of_data <- box_scores[agrep(final_table$Player[i], box_scores$athlete_display_name),]
  }
  
  # If there's still no match, do a more lax fuzzy match
  if(nrow(full_player_subset_of_data) == 0){
    print(paste0("going to second level of fuzzy match for: ", final_table$Player[i]))
    full_player_subset_of_data <- box_scores[agrep(final_table$Player[i], box_scores$athlete_display_name, max.distance = 2),]
  }
  
  # If there's STILL no match, skip them
  if(nrow(full_player_subset_of_data) == 0){
    print(paste0("skipping: ", final_table$Player[i]))
    next
  }
  
  # Create a column of the matchup
  for(j in 1:nrow(full_player_subset_of_data)){
    full_player_subset_of_data$matchup[j] <- paste0(unique(box_scores[which(box_scores$game_id==full_player_subset_of_data$game_id[j])]$team_abbreviation), collapse = '-')
  }
  
  # Update the matchup names and remove those that aren't real teams (e.g., all star game)
  matchup_df <- data.frame(str_split_fixed(full_player_subset_of_data$matchup, "-", 2))
  matchup_df <- cbind(matchup_df, 1:nrow(matchup_df))
  colnames(matchup_df) <- c("Away", "Home", "Order")
  matchup_df <- merge(x=matchup_df, y=away_hoopR_team_abbreviations, by="Away", all.x=TRUE, sort = FALSE)
  matchup_df <- merge(x=matchup_df, y=home_hoopR_team_abbreviations, by="Home", all.x=TRUE)
  matchup_df <- matchup_df[order(matchup_df$Order),]

  full_player_subset_of_data$matchup <- paste(matchup_df$AwayTeam, matchup_df$HomeTeam, sep="-")
  
  full_player_subset_of_data <- full_player_subset_of_data[!is.na(full_player_subset_of_data$matchup),]
  
  # Get the last 20 games
  temp_player_subset_of_data <- full_player_subset_of_data[1:20,]
  
  # Get the relevant stats for the player
  # Points
  # Assists
  # Rebounds
  # Min / game
  # FGA & FMG
  # 3PM - 3PA
  # Position
  # Opponent
  player_subset_of_data <- temp_player_subset_of_data[, c(1, 26, 3, 16, 4, 5, 9, 10, 35, 36)]
  colnames(player_subset_of_data) <- c("name", "position", "min", "pts", "fg", "fg3", "reb", "ast", "game_date", "matchup")
  
  player_subset_of_data$game_date <- as.character(player_subset_of_data$game_date)
  
  # Put the player data into the final_table
  for(j in 1:nrow(player_subset_of_data)){
    final_table[i, paste0("regular_", j, "_", temp_colnames_player)]  <- player_subset_of_data[j, ]
  }
  
  # Opponent
  opponent <- setdiff(c(final_table[i, c("Away", "Home")]), final_table$Team[i])[[1]]
  final_table$Opponent[i] <- opponent
  
  # Player vs opponent
  temp_opponent_subset_of_data <- full_player_subset_of_data[grep(opponent, full_player_subset_of_data$matchup),]
  opponent_subset_of_data <- temp_opponent_subset_of_data[1:5, c(1, 26, 3, 16, 4, 5, 9, 10, 35, 36)]
  colnames(opponent_subset_of_data) <- c("name", "position", "min", "pts", "fg", "fg3", "reb", "ast", "game_date", "matchup")
  
  opponent_subset_of_data$game_date <- as.character(opponent_subset_of_data$game_date)
  
  # Put the opponent data into the final_table
  for(j in 1:nrow(opponent_subset_of_data)){
    final_table[i, paste0("opponent_", j, "_", temp_colnames_player)]  <- opponent_subset_of_data[j, ]
  }
}

## Pull in the Potential Assists 
today <- Sys.Date()
pot_assists <- read.csv(paste0(today, ".csv"))

# Downselect to the helpful columns
potential_assists <- pot_assists[, c("PLAYER", "PotentialAST")]
colnames(potential_assists) <- c("Player", "PotentialAssists")

# Merge into the final table
final_table <- merge(x=final_table, y=potential_assists, by="Player", all.x=TRUE, all.y=FALSE)

## Pull in the Matchup data
matchups_raw <- read.csv(paste0(today, ".csv"))

team_names_matchup <- read.csv("TeamNamesToMatchups.csv")

# Add in the team name abbreviation and extract the position
matchups_raw <- merge(x=matchups_raw, y=team_names_matchup, by.x="Team", by.y="TeamMatchup", all.x=TRUE)
matchups_raw$Position <- unlist(lapply(str_split(matchups_raw$TimeFrame, " "), "[", 2))
matchups_raw$TimeFrame <- unlist(lapply(str_split(unlist(lapply(str_split(matchups_raw$TimeFrame, " "), "[", 1)), "-"), "[", 2))

# Downselect to the helpful columns
matchups_raw <- matchups_raw[, c("TeamShort", "Position", "PTS", "REB", "AST", "X3PM", "FG.", "TimeFrame")]
colnames(matchups_raw) <- c("Team", "Position", "Matchup_Points", "Matchup_Rebounds", "Matchup_Assists", "Matchup_3PM", "Matchup_FG%", "Last_X_Games")

# Remove the 0 row from matchups_raw
matchups_raw <- matchups_raw[which(matchups_raw$Last_X_Games != "0"), ]

# Convert from long to wide
reshaped_matchups <- reshape(matchups_raw, direction="wide", idvar=c("Team", "Position"), timevar="Last_X_Games")
colnames(reshaped_matchups)[1] <- "Opponent"

# Merge into the final table
final_table <- merge(x=final_table, y=reshaped_matchups, by.x=c('Opponent', 'regular_1_position'), by.y=c('Opponent', 'Position'), all.x=TRUE)

## Save the file
# Archive it
write.csv(final_table, paste0("FinalTables/", today, ".csv"))
