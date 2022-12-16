
## NBA Potential Assists Scraper
## December 2022
## rha

## Load Libraries & SetWD
library(RSelenium)
library(dplyr)
library(rvest)

# Set up the final dataset
final_table <- data.frame(matrix(ncol = 16, nrow = 0))
colnames(final_table) <- c("PLAYER", "TEAM", "GP", "W", "L", "MIN", "PassesMade", "PassesReceived", "AST", "SecondaryAST", "PotentialAST", "AST PTSCreated", "AST PTSCreated", "ASTAdj", "AST ToPass%", "AST ToPass% Adj")

# Open Selenium in headless mode
port <- 4568L
driver <- rsDriver(browser = "firefox", check = FALSE, verbose = FALSE, port = port,
                   extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless'))))
remDr <- driver[["client"]]

# Navigate to the page to scrape
remDr$navigate('https://www.nba.com/stats/players/passing?CF=POTENTIAL_AST*G*0&dir=D&sort=POTENTIAL_AST')

i <- 0
# In a loop 5 times, get the data on the assisters and add it othe final table
while(i < 5){
  # Get the table
  table <- remDr$findElement(using = 'xpath', value = '/html/body/div[1]/div[2]/div[2]/div[3]/section[2]/div/div[2]/div[3]/table')
  temp_df = table$getPageSource()[[1]]%>% 
    read_html() %>%
    html_table()
  
  # Append results to the final_table dataframe
  final_table <- rbind(final_table, temp_df[[3]])
  
  # Hit the next page button
  button <- remDr$findElement(using='xpath', value='/html/body/div[1]/div[2]/div[2]/div[3]/section[2]/div/div[2]/div[2]/div[1]/div[6]/button[2]')
  button$sendKeysToElement(list(key = "enter"))
  
  # Update i
  i <- i + 1
  
  # Sleep for between 20 and 30 seconds
  Sys.sleep(runif(1, 20, 30))
}

# Close the Selenium browser and stop the server
remDr$close()
driver$server$stop()
driver$server$process

# Get today's date & month
today <- Sys.Date()

# Save the dataframe of potential assists to the right location
write.csv(final_table, paste0('PotentialAssists/'today, ".csv"))
