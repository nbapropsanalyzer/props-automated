
## NBA Potential Assists Scraper
## December 2022
## rha

## Load Libraries & SetWD
library(RSelenium)
library(dplyr)
library(rvest)

# Set up the final dataset
# final_table <- data.frame(matrix(ncol = 16, nrow = 0))
# colnames(final_table) <- c("PLAYER", "TEAM", "GP", "W", "L", "MIN", "PassesMade", "PassesReceived", "AST", "SecondaryAST", "PotentialAST", "AST PTSCreated", "AST PTSCreated", "ASTAdj", "AST ToPass%", "AST ToPass% Adj")


# Open Selenium in headless mode
port <- 4568L
driver <- rsDriver(browser = "firefox", check = FALSE, verbose = FALSE, port = port,
                   extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless'))))
remDr <- driver[["client"]]

# Navigate to the page to scrape
remDr$navigate('https://www.fantasypros.com/nba/defense-vs-position.php')

table <- remDr$findElement(using = 'xpath', value = '/html/body/div[2]/div[4]/div/div[1]/div/div[5]')
temp_df = table$getPageSource()[[1]]%>% 
  read_html() %>%
  html_table()


# Stats df
stats_df <- temp_df[[1]]

# New column to add
stats_df <- cbind(stats_df, rep(c("GC-0 All", "GC-7 All", "GC-15 All", "GC-30 All", 
  "GC-0 C", "GC-7 C", "GC-15 C", "GC-30 C", 
  "GC-0 PF", "GC-7 PF", "GC-15 PF", "GC-30 PF", 
  "GC-0 PG", "GC-7 PG", "GC-15 PG", "GC-30 PG", 
  "GC-0 SF", "GC-7 SF", "GC-15 SF", "GC-30 SF", 
  "GC-0 SG", "GC-7 SG", "GC-15 SG", "GC-30 SG", 
  "GC-0 TM", "GC-7 TM", "GC-15 TM", "GC-30 TM"), times=30))

colnames(stats_df) <- c("Team", "PTS", "REB", "AST", "3PM", "STL", "BLK", "TO", "FG%", "FT%", "TimeFrame")

# Close the Selenium browser and stop the server
remDr$close()
driver$server$stop()
driver$server$process

# Get today's date & month
today <- Sys.Date()

# Save the dataframe of potential assists to the right location
write.csv(stats_df, paste0("Matchups/", today, ".csv"))
