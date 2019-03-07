#statcast dashboard

#run separate function to fetch data
# to update existing db

# setwd("")
library(data.table)
library(magrittr)
library(lubridate)
library(DBI)
library(RSQLite)
library(shiny)
library(DT)
library(ggplot2)

#lookup tables ####

#point to statcast sqlite db
db_file <- "data/statcast_updated.sqlite"

statcast_leaderboard <- readRDS("data/statcast_leaderboards(2015-2018).rds")

#playerid map from statcast leaderboard
players_lookup <- statcast_leaderboard[, .(player_id = as.numeric(player_id), name)] %>% unique %>% setkey(player_id)
# ----

db <- dbConnect(RSQLite::SQLite(), db_file)

db_table <- dbListTables(db)

#pitch type - note, already in data - NOTE - or only in new data? - save for future reference if needed - likely better way to do this
# pitches <-
# 	c(
# 	"AB","Automatic_Ball",
# 	"AS","Automatic_Strike",
# 	"CH","Changeup",
# 	"CU","Curveball",
# 	"EP","Eephus",
# 	"FC","Cutter",
# 	"FF","Four_Seam_Fastball",
# 	"FO","Forkball",
# 	"FS","Splitter",
# 	"FT","Two_seam_Fastball",
# 	"GY","Gyroball",
# 	"IN","Intentional_Ball",
# 	"KC","Knuckle_curve",
# 	"KN","Knuckleball",
# 	"PO","Pitch_Out",
# 	"SC","Screwball",
# 	"SI","Sinker",
# 	"SL","Slider",
# 	"UN","Unidentified"
# )

# pitch_type_list <- list()

# for (i in seq(1, length(pitches), 2)){
# 	pitch_type_list[[i]] <- data.table(pitch_type = pitches[i], pitch_type_des = pitches[i+1])
# }

# pitch_type <- rbindlist(pitch_type_list)
# ----

##shiny pitcher game summary ####

#function to merge player names - FUTURE - combine into one function?
#place name at front
add_pitcher_name <- function(table){
	temp <- merge(players_lookup, table, by.x = "player_id", by.y = "pitcher", all.y = TRUE) %>% setDT()

	return(temp)
}

add_batter_name <- function(table){
	temp <- merge(players_lookup, table, by.x = "player_id", by.y = "batter", all.y = TRUE) %>% setDT()

	return(temp)
}

# ----

ui <- fluidPage(
  titlePanel("Pitcher analysis"),

  sidebarLayout(
    sidebarPanel(
		uiOutput("date_range_output"),

		actionButton("query_all_pitchers", "Find pitchers"),

		uiOutput("pitcher_output"),

		actionButton("go", "Filter pitcher data"),

		radioButtons("view_input", "View by", choices = c("raw", "z-score", "percentile"), selected = "raw")

		),

      mainPanel(DTOutput("results"))

))

# ----


server <- function(input, output) {

db <- dbConnect(RSQLite::SQLite(), db_file)
db_table <- dbListTables(db)

min_date_in_db <- dbGetQuery(db, paste0('
  SELECT
  min(game_date)
  FROM ', db_table)) %>% .[[1]] %>% as_date

max_date_in_db <- dbGetQuery(db, paste0('
  SELECT
  max(game_date)
  FROM ', db_table)) %>% .[[1]] %>% as_date

output$date_range_output <- renderUI({
	dateRangeInput("date_range_input", "Set Date Range",
    start = min_date_in_db, end = max_date_in_db)
})

#cols to z score percentile
cols_to_scale <- c(
	"avg_mph",
	"avg_rpm",
	"avg_ev",
	"rpm_mph",
	"pct",
	"strike_pct",
	"swstr_pct"
)

#find pitchers
all_pitcher_aggegrate_data <- eventReactive(input$query_all_pitchers, {
	dbGetQuery(db, paste0('
	SELECT
	pitcher,
	pitch_type,
	pitch_name,
	count(pitch_number) as count,
	avg(release_speed) as avg_mph,
	avg(release_spin_rate) as avg_rpm,
	avg(launch_speed) as avg_ev,
	/* count(CASE
		when description in ("ball", "blocked_ball", "hit_by_pitch", "pitchout", "intent_ball") then 1
		else NULL
		END) as ct_balls, */
	count(CASE
		when description not in ("ball", "blocked_ball", "hit_by_pitch", "pitchout", "intent_ball") then 1
		else NULL
		END) as ct_strikes,
	count(CASE
		when description in ("foul_tip", "swinging_strike", "swinging_strike_blocked") then 1
		else NULL
		END) as ct_swstr
	FROM ', db_table, '
		WHERE pitch_type not in ("", "null") and
		game_date BETWEEN ', as.numeric(input$date_range_input[1]), ' AND ', as.numeric(input$date_range_input[2]), '
	group by pitcher, pitch_type'
		)) %>% add_pitcher_name() %>%
		#add in % calculations - could make function?
		.[, rpm_mph:= avg_rpm/avg_mph] %>%
		.[, pct:= count / sum(count), player_id] %>%
		.[, strike_pct:= ct_strikes / count] %>%
		.[, swstr_pct:= ct_swstr / count] %>%

		#add in zscore and percentile conversion - by pitch type
		.[, paste0("z_", cols_to_scale):= lapply(.SD, function(x) round(scale(x)[,1], digits=3)), by = "pitch_type", .SDcols = cols_to_scale ] %>%

		#calc percentile (pnorm)
		.[, paste0("p_", cols_to_scale):= lapply(.SD, function(x) round(pnorm(x) * 100, digits=0)), by = "pitch_type", .SDcols = paste0("z_", cols_to_scale) ]

})

output$pitcher_output <- renderUI({
	selectInput("pitcher_input", "Select Pitcher",
    c("*All*", all_pitcher_aggegrate_data()[, (name)]) %>% sort, multiple = TRUE)
})

pitcher_ids <- reactive(
	if("*All*" %in% input$pitcher_input) {
		players_lookup[, (player_id)]
		} else {
		players_lookup[name %in% input$pitcher_input, (player_id)]
		}
)
#FUTURE - make more dynamic
display_cols <- reactive(if(input$view_input == "raw") {
	c("name",
	"pitch_name",
	"count",
	"pct",
	"avg_mph",
	"avg_rpm",
	"rpm_mph",
	"avg_ev",
	"strike_pct",
	"swstr_pct")
} else if(input$view_input == "z-score") {
	c("name",
	"pitch_name",
	"count",
	"z_pct",
	"z_avg_mph",
	"z_avg_rpm",
	"z_rpm_mph",
	"z_avg_ev",
	"z_strike_pct",
	"z_swstr_pct")
} else if(input$view_input == "percentile") {
	c("name",
	"pitch_name",
	"count",
	"p_pct",
	"p_avg_mph",
	"p_avg_rpm",
	"p_rpm_mph",
	"p_avg_ev",
	"p_strike_pct",
	"p_swstr_pct"
	)
})

pitcher_data <- eventReactive(input$go, {
	all_pitcher_aggegrate_data()[player_id %in% pitcher_ids()] %>%
		setorder(name, -count)

})

#FUTURE - new query to get all pitches from selected pitcher - viz by game/month/year?

final <- reactive(pitcher_data()[, display_cols(), with = FALSE])

#FUTURE - create ability to compare/viz pitchers to each other and league

#FUTURE - better way to format numbers? - way to minimize column padding?
output$results <- renderDT({
    if(input$view_input == "raw"){
	DT::datatable(final(), rownames = FALSE, selection = "none"
      , filter = 'top', class = "compact stripe", options = list(pageLength = 25)) %>%
	  formatPercentage(c("pct", "strike_pct", "swstr_pct"), 1) %>%
	  formatRound(c("avg_mph", "avg_ev", "rpm_mph"), 1) %>%
	  formatRound("avg_rpm", 0)
	} else {
	DT::datatable(final(), rownames = FALSE, selection = "none"
      , filter = 'top', class = "compact stripe", options = list(pageLength = 25))}
  })
}
# ----

runApp(shinyApp(ui = ui, server = server))
