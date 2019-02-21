#scrape statcast leaderboards to get playerid

#only need to do once (unless new players call-ups missing from current season)

#samples
# https://baseballsavant.mlb.com/statcast_leaderboard?year=2017&abs=0&player_type=resp_batter_id

# https://baseballsavant.mlb.com/statcast_leaderboard?year=2018&abs=300&player_type=pitcher

# seasons = 2015 - 2018
# ----

library(data.table)
library(magrittr)
library(jsonlite)
library(xml2)

# ----

scrape_statcast <- function(season, type){#type is "resp_batter_id" or "pitcher" or "batter" -- not sure why front end uses first batter

	#testing
	# url <- paste0("https://baseballsavant.mlb.com/statcast_leaderboard?year=", 2018, "&abs=500&player_type=", "batter")

	url <- paste0("https://baseballsavant.mlb.com/statcast_leaderboard?year=", season, "&abs=0&player_type=", type)

	pg <- read_html(url)

	nodes <- pg %>% xml_find_all(".//script")

	find_data <- grep("leaderboard_data", nodes)

	raw_data <- nodes[find_data] %>% xml_text

	partial_json_data <- strsplit(raw_data, " leaderboard_data = ")[[1]][[2]]

	json_data <- strsplit(partial_json_data, ";")[[1]][[1]]

	output <- fromJSON(json_data)
}

seasons_to_scrape <- 2015:2018

table_scrape_values <- list(
	data.table(season = seasons_to_scrape, type = "pitcher"),
	data.table(season = seasons_to_scrape, type = "batter")
 )%>% rbindlist()

results_from_scrape <- mapply(scrape_statcast, table_scrape_values$season, table_scrape_values$type, SIMPLIFY = FALSE)

results <- rbindlist(results_from_scrape)

#save as rds for now
#NOTE - need change formats

saveRDS(results, "data/statcast_leaderboards(2015-2018).rds")

#need to save off playerid map?

#FUTURE - use as in-season tool to scrape leaderboards?
#- save to db?