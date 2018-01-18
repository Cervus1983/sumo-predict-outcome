library(tidyverse)
library(stringr)

# data
banzuke <- read_csv("banzuke.csv")
results <- read_csv("results.csv")
odds <- read_csv("odds.csv")

# functions
source("common.R")

sapply(list.files("v2_functions", full.names = TRUE), source)

# parses rank (Y1e) into rank_name (Y) + rank_level (1)
parse_rank <- function(data) data %>% 
	mutate(
		rikishi1_rank_name = ordered(str_extract(rikishi1_rank, "^\\D+"), levels = all_ranks),
		rikishi1_rank_level = as.integer(str_extract(rikishi1_rank, "\\d+")),
		rikishi1_rank_name_prev = ordered(str_extract(rikishi1_prev, "^\\D+"), levels = all_ranks),
		rikishi1_rank_level_prev = as.integer(str_extract(rikishi1_prev, "\\d+")),
		
		rikishi2_rank_name = ordered(str_extract(rikishi2_rank, "^\\D+"), levels = all_ranks),
		rikishi2_rank_level = as.integer(str_extract(rikishi2_rank, "\\d+")),
		rikishi2_rank_name_prev = ordered(str_extract(rikishi2_prev, "^\\D+"), levels = all_ranks),
		rikishi2_rank_level_prev = as.integer(str_extract(rikishi2_prev, "\\d+"))
	)

add_form <- function(data) data %>% 
	group_by(basho, rikishi1_id) %>% 
	mutate(
		rikishi1_form = lag(rikishi1_result, order_by = day),
		rikishi1_form = ifelse(is.na(rikishi1_form), "0-0", str_extract(rikishi1_form, "\\d+-\\d+"))
	) %>% 
	group_by(basho, rikishi2_id) %>% 
	mutate(
		rikishi2_form = lag(rikishi2_result, order_by = day),
		rikishi2_form = ifelse(is.na(rikishi2_form), "0-0", str_extract(rikishi2_form, "\\d+-\\d+"))
	)

# rich data set
all_data <- results %>% 
	add_banzuke_info() %>% 
	add_current_basho_performance() %>% 
	add_head_to_head() %>% 
	left_join(., odds)

all_data <- results %>% 
	add_banzuke_info() %>% 
	parse_rank() %>% 
	add_form() %>% 
	add_head_to_head() %>% 
	left_join(., odds)

data_to_model <- function(data) data %>% 
	filter(
		basho > 1989, basho < 2018.01, # remove first 6 years & current basho
		kimarite != "fusen" # remove walkovers
	) %>% 
	# birth date -> age (in years, on the 1st of the month)
	mutate(
		rikishi1_age = as.integer(difftime(
			as.Date(paste(basho, "01", sep = "."), format = "%Y.%m.%d"),
			as.Date(rikishi1_birth_date, format = "%d.%m.%Y"),
			units = "days"
		)) / 365.25,
		rikishi2_age = as.integer(difftime(
			as.Date(paste(basho, "01", sep = "."), format = "%Y.%m.%d"),
			as.Date(rikishi2_birth_date, format = "%d.%m.%Y"),
			units = "days"
		)) / 365.25
	) %>% 
	# drop extraneous columns
	select(-one_of(c(
		"day",
		"rikishi1_id", "rikishi1_rank", "rikishi1_shikona", "rikishi1_result", "rikishi1_birth_date", "rikishi1_prev",
		"kimarite",
		"rikishi2_id", "rikishi2_rank", "rikishi2_shikona", "rikishi2_result", "rikishi2_win", "rikishi2_birth_date", "rikishi2_prev"
	))) %>% 
	# target variable
	mutate(rikishi1_win = recode(rikishi1_win + 1, "no", "yes"))

all_data %>% 
	filter(is.na(odds1)) %>% 
	data_to_model() %>% 
	select(-basho) %>% 
	select(-contains("odds")) %>% 
	write_csv(., "train.csv")

all_data %>% 
	filter(!is.na(odds1)) %>% 
	data_to_model() %>% 
	select(-basho) %>% 
	select(-contains("odds")) %>% 
	write_csv(., "test.csv")
