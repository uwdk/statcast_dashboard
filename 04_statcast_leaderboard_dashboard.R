#shiny dash for statcast leaderboard

library(data.table)
library(magrittr)
library(lubridate)
library(DBI)
library(RSQLite)
library(shiny)
library(DT)
library(ggplot2)

#load data and cleanup
import_files <- list.files("data/", pattern = "leaderboard", full.names = TRUE)

leaderboard_data <- lapply(import_files, readRDS) %>% rbindlist(use.names = TRUE, fill = TRUE)

#convert col types
convert_numeric_cols <- function(table){

	#find cols w/alpha characters - to ignore
	#NOTE- probably can do this better
	skip_cols <- sapply(names(table), function(x) grepl("[A-Za-z]", table[[x]]) %>% any()) %>% grep(TRUE, .) %>% names(table)[.]

	output <- table[, lapply(.SD, as.numeric), skip_cols]
}

leaderboard_data %<>% convert_numeric_cols()

setcolorder(leaderboard_data, c("name", "player_id", "season"))

leaderboard_data[, c("rowId", "pitcher"):= NULL]

leaderboard_data[player_type %like% "batter", player_type:= "batter"]

# ----

#shiny dashboard
ui <- fluidPage(
  titlePanel("Statcast leaderboard"),

  sidebarLayout(
    sidebarPanel(
		#set min attempts to filter data for calculations
		radioButtons("type_input", "Type", choices = c("batter", "pitcher"), selected = "batter"),

		numericInput("min_attempts_input", "Set minimum threshold", 0),

		uiOutput("player_output"),

		radioButtons("view_input", "View by", choices = c("raw", "z-score", "percentile"), selected = "raw")

		),

      mainPanel(DTOutput("results"))

))

server <- function(input, output) {
	cols_to_scale <- sapply(names(leaderboard_data), function(x) grepl("[A-Za-z]", leaderboard_data[[x]]) %>% any()) %>% grep(FALSE, .) %>% names(leaderboard_data)[.]  %>% .[.!="season"] %>% .[.!="player_id"]

	data <- reactive(
		leaderboard_data[attempts >= input$min_attempts_input & player_type == input$type_input] %>%

		#add in zscore and percentile conversion
		.[, paste0("z_", cols_to_scale):= lapply(.SD, function(x) round(scale(x)[,1], digits=3)), .SDcols = cols_to_scale ] %>%

		#calc percentile (pnorm)
		.[, paste0("p_", cols_to_scale):= lapply(.SD, function(x) round(pnorm(x) * 100, digits=0)), .SDcols = paste0("z_", cols_to_scale) ]
	)

	output$player_output <- renderUI({
	selectInput("player_input", "Select Player",
    c("*All*", data()[, (name)]) %>% sort, multiple = TRUE)
	})

	names_to_filter <- reactive(
		if("*All*" %in% input$player_input) {
			data()[, (name)]
			} else {
			data()[name %in% input$player_input, (name)]
			}
	)

	#FUTURE - make more dynamic
	display_cols <- reactive(if(input$view_input == "raw") {
		c("name",
		"season",
		cols_to_scale)
	} else if(input$view_input == "z-score") {
		c("name",
		"season",
		paste0("z_", cols_to_scale))
	} else if(input$view_input == "percentile") {
		c("name",
		"season",
		paste0("p_", cols_to_scale))
	})

	output$results <- renderDT({
    DT::datatable(data()[name %in% names_to_filter(), display_cols(), with = FALSE], rownames = FALSE, selection = "none"
      , filter = 'top', class = "compact stripe", options = list(pageLength = 25))
	})
}

runApp(shinyApp(ui = ui, server = server))
