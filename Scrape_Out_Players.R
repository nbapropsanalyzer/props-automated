

## NBA Potential Assists Scraper
## December 2022
## rha

## Load Libraries & SetWD
library(rvest)
library(stringr)

roto_abbreviations <- read.csv("TeamNamesToRotoWire.csv", stringsAsFactors = FALSE)
colnames(roto_abbreviations) <- c("Original", "Roto")

# Get the 'out' players from the lineup 
roto_url <- paste0("https://www.rotowire.com/basketball/nba-lineups.php")

# Get the roster table
roto_html <- xml2::read_html(roto_url)

# Get the index within the roto url that contains the correct game
all_away_team <- unlist(lapply(str_extract_all(roto_html %>% html_elements('.lineup__box') %>% html_elements('.lineup__teams') %>% html_text2(), "[A-Z]{3}"), "[[", 1))

# Select an away team
away_team <- all_away_team[1]
roto_index <- which(all_away_team == roto_abbreviations$Roto[which(roto_abbreviations$Original == away_team)])

# Get the 0% & 25% likelikhood players
all_games_info <- roto_html %>% html_elements('.lineup__box')
away_0_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-visit') %>% html_elements('.is-pct-play-0') %>% html_text2(), "\r"), function(x) x[length(x) -1]))
home_0_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-home') %>% html_elements('.is-pct-play-0') %>% html_text2(), "\r"), function(x) x[length(x) -1]))
away_25_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-visit') %>% html_elements('.is-pct-play-25') %>% html_text2(), "\r"), function(x) x[length(x) -1]))
home_25_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-home') %>% html_elements('.is-pct-play-25') %>% html_text2(), "\r"), function(x) x[length(x) -1]))
away_50_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-visit') %>% html_elements('.is-pct-play-50') %>% html_text2(), "\r"), function(x) x[length(x) -1]))
home_50_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-home') %>% html_elements('.is-pct-play-50') %>% html_text2(), "\r"), function(x) x[length(x) -1]))
away_75_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-visit') %>% html_elements('.is-pct-play-75') %>% html_text2(), "\r"), function(x) x[length(x) -1]))
home_75_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-home') %>% html_elements('.is-pct-play-75') %>% html_text2(), "\r"), function(x) x[length(x) -1]))
away_100_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-visit') %>% html_elements('.is-pct-play-100') %>% html_text2(), "\r"), function(x) x[length(x) -1]))
home_100_players <- unlist(lapply(str_split(all_games_info[[roto_index]] %>% html_elements('.is-home') %>% html_elements('.is-pct-play-100') %>% html_text2(), "\r"), function(x) x[length(x) -1]))



