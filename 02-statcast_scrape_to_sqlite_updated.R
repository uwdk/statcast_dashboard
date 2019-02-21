#statcast/pitchfx scrape
#for 2018 - combine scrape w/data prep and populate sqlite db

# https://github.com/BillPetti/baseballr/tree/master/R
#
# https://github.com/BillPetti/baseballr/blob/master/R/scrape_statcast_savant_batter_all.R
# pitchers is same just that player name is different


#' Query Statcast and PITCHf/x Data for All Batters from baseballsavant.mlb.com
#'
#' This function allows you to query Statcast and PITCHf/x data as provided on baseballsavant.mlb.com and have that data returned as a dataframe. Query returns data for all batters over a given time frame.
#' @param start_date Date of first game for which you want data. Format must be in Y-d-m format.
#' @param end_date Date of last game for which you want data. Format must be in Y-d-m format.
#' @keywords MLB, sabermetrics, Statcast
#' @importFrom utils read.csv
#' @export
#' @examples
#' \dontrun{
#' scrape_statcast_savant_batter_all(start_date = "2016-04-06", end_date = "2016-04-15")
#' }

# ----

library(DBI)
library(data.table)
library(lubridate)
library(magrittr)


db_filename <- file.path(getwd(), "data", "statcast_updated.sqlite")
db_table_name <- "statcast"

# ----

db_load <- dbConnect(RSQLite::SQLite(), db_filename)

# ----
scrape_statcast_savant_batter_all <- function(start_date, end_date=start_date) {
  # extract season from start_date
  year <- substr(start_date, 1,4)

  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C&hfC=&hfSea=", year, "%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=",start_date,"&game_date_lt=",end_date,"&team=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&min_abs=0&type=details&")

  # Do a try/catch to show errors that the user may encounter while downloading.
  tryCatch(
    {
    #   print("These data are from BaseballSevant and are property of MLB Advanced Media, L.P. All rights reserved.")
    #   print("Grabbing data, this may take a minute...")
      cat("Fetching data from", start_date, "to", end_date, "\n")

      # payload <- utils::read.csv(url) #change to fread?
      payload <- fread(url, na.strings = "null")

    },
    error=function(cond) {
      message(paste("URL does not seem to exist, please check your Internet connection:"))
      message("Original error message:")
      message(cond)
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning. Make sure your date range is correct:"))
      message("Original warning message:")
      message(cond)
      return(NULL)
    }
  )
	# NOTE - #do conversions after? - some aren't needed for sqlite format
  # Clean up formatting.
  # payload[payload=="null"] <- NA
#   payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
  # payload$des <- as.character(payload$des)
  # payload$game_pk <- as.character(payload$game_pk) %>% as.numeric()
  # payload$on_1b <- as.character(payload$on_1b) %>% as.numeric()
  # payload$on_2b <- as.character(payload$on_2b) %>% as.numeric()
  # payload$on_3b <- as.character(payload$on_3b) %>% as.numeric()
  # payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
  # payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
  # payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as.numeric()
#   payload$launch_speed <- as.character(payload$launch_speed) %>% as.numeric()
#   payload$launch_angle <- as.character(payload$launch_angle) %>% as.numeric()
  # payload$effective_speed <- as.character(payload$effective_speed) %>% as.numeric()
  # payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as.numeric()
  # payload$release_extension <- as.character(payload$release_extension) %>% as.numeric()
  # payload$barrel <- with(payload, ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 11 & launch_speed + launch_angle >= 124, 1, 0))
#   payload[launch_angle <= 50 &
#             launch_speed >= 98 &
#             launch_speed * 1.5 - launch_angle >= 11 &
#             launch_speed + launch_angle >= 124,
#           barrel:= 1]


  message("URL read and payload aquired successfully.")

  return(payload)

}

#NOTE - will error on first run - just used for reference
all_dates_in_db <- dbGetQuery(db_load, paste0("SELECT game_date, count (distinct game_pk) as games, count(game_pk) as pitches FROM ", db_table_name, " group by game_date"))

setDT(all_dates_in_db)
all_dates_in_db[, game_date:= as_date(game_date)]

#create date ranges - scrape in 6 day increments - b/c max at 30k records

#search for max date in db - query to present (yesterday b/c of how data populated)
# max_date_in_db <- dbGetQuery(db_load, 'SELECT max(game_date) FROM statcast_2018')
#
#
# set_first_day <- max_date_in_db %>% as.character
# set_last_day <- (today() - 1) %>% as.character

# set_first_day <- "2018-03-30" %>% as.Date #opening day "2017-04-02"
# set_last_day <- "2018-03-31" %>% as.Date
# set_last_day <- ceiling_date(set_first_day, "month")-1

#find date range increments
date_range_to_scrape <- function(start = NULL, end = NULL){ #MM-DD-YYYY
  if (is.null(start)){
    max_date_in_db <- dbGetQuery(db_load, paste0("SELECT max(game_date) FROM ", db_table_name)) %>% .[[1]] %>% as_date()
    first_of_date_range <- max_date_in_db + 1
  } else {
      first_of_date_range <- mdy(start)
  }

  if (is.null(end)){
  last_of_date_range <- today() - 1
  } else {
    last_of_date_range <- mdy(end)
  }

  seq(first_of_date_range, last_of_date_range, 1)
}

#NOTE - on first run, need to specify dates
# dates_all <- date_range_to_scrape("3/29/2018", "4/4/2018") #example format w/dates - "MM/DD/YYYY"

#full season dates
# 2018 - ("3/29/2018", "10/31/2018")
# 2017 - ("4/2/2017", "10/29/2017")
# 2016 - ("4/3/2016", "11/2/2016")
# 2015 - ("4/5/2015", "11/1/2015")

dates_all <- date_range_to_scrape() #blank to update data (day after last date in db through yesterday)


#create table w/date ranges
create_date_table <- function(dates){
    if (length(dates) <= 6){
    date_range_table <- data.table(start_date = min(dates), end_date = max(dates))
  } else {#NOTE - need to create rows w/6 digit increments
    scrape_groups <- ceiling(length(dates) / 6)
    temp_table <- split(dates, cut(dates, scrape_groups))
    date_range_table <- data.table(start_date = lapply(temp_table, function(x) as.character(min(x))), end_date = lapply(temp_table, function(x) as.character(max(x)))) %>% .[, lapply(.SD, as.character)]
  }

  date_range_table[, start_date:= as.character(start_date)]
  date_range_table[, end_date:= as.character(end_date)]

  return(date_range_table)
}

date_range_table <- create_date_table(dates_all)

#start scrape
results_from_scrape <- mapply(scrape_statcast_savant_batter_all, date_range_table$start_date, date_range_table$end_date, SIMPLIFY = FALSE)

results <- rbindlist(results_from_scrape)

# ----

#data prep?
# type conversions? - not needed for sqlite load?
# fix null and unknown to NA - done w/fread NA option

#clean data ####
setnames(results, make.names(names(results), unique = T))

results[, game_date:= ymd(game_date)] #to convert to date format - allows for filtering when query by date range

#add barrel
results[, launch_speed:= as.character(launch_speed) %>% as.numeric()]
results[, launch_angle:= as.character(launch_angle) %>% as.numeric()]

results[launch_angle <= 50 &
		launch_speed >= 98 &
		launch_speed * 1.5 - launch_angle >= 11 &
		launch_speed + launch_angle >= 124,
		barrel:= 1]

# ----

# save to sqlite ####
#to append data
dbWriteTable(db_load, db_table_name, results, append = TRUE)

#add index - might not need individual ones

dbSendQuery(db_load, paste0("CREATE INDEX idx_pitcher ON ", db_table_name, " (pitcher)")) %>% dbClearResult()

dbSendQuery(db_load, paste0("CREATE INDEX idx_game_date ON ", db_table_name, " (game_date)")) %>% dbClearResult()

dbSendQuery(db_load, paste0("CREATE INDEX idx_game_year	ON ", db_table_name, "(game_year)")) %>% dbClearResult()

dbSendQuery(db_load, paste0("CREATE INDEX idx_pitch_name ON ", db_table_name, "(pitch_name)")) %>% dbClearResult()

dbSendQuery(db_load, paste0("CREATE INDEX idx_pitch_type ON ", db_table_name, "(pitch_type)")) %>% dbClearResult()

dbSendQuery(db_load, paste0("CREATE INDEX idx_description ON ", db_table_name, "(description)")) %>% dbClearResult()

dbSendQuery(db_load, paste0("CREATE INDEX idx_multi ON ", db_table_name, " (game_date, pitcher, pitch_type, description)")) %>% dbClearResult()


#close db
dbDisconnect(db_load)
