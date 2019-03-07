#master file for statcast data dashboard

setwd("")

# ----

##step 1 - update statcast data ####

#scrape statcast leaderboard - used as playerid map
#NOTE - doesn't need to be updated everytime -- or could modify to only update current year
source("01-scrape_statcast_leaderboard.R")

#update statcast data - will check current db to find new dates needed - run manually first time to set date ranges
source("02-statcast_scrape_to_sqlite_updated.R")

# ----

#step 2- data prep and load to shiny ####
#FUTURE - split into separate data prep, shiny ui/server scripts?
source("03-statcast_dashboard.R")

#shiny dash of leaderboard ####
source("04_statcast_leaderboard_dashboard.R")