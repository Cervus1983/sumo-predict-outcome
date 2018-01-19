library(stringr)
library(tidyverse)


# data
banzuke <- read_csv("banzuke.csv")
results <- read_csv("results.csv")
odds <- read_csv("odds.csv")


# functions
source("common.R")
source("features.R")


# complete data set
all_data <- results %>% 
	add_banzuke_info(banzuke) %>% 
	add_age() %>% 
	parse_rank() %>% 
	add_form() %>% 
	add_head_to_head() %>% 
	left_join(., odds)


# drops extraneous columns
drop_extra_cols <- function(data) data %>% 
	select(
		-one_of(
			c(
				"basho", "day",
				"rikishi1_id", "rikishi1_rank", "rikishi1_shikona", "rikishi1_result", "rikishi1_birth_date", "rikishi1_prev",
				"kimarite",
				"rikishi2_id", "rikishi2_rank", "rikishi2_shikona", "rikishi2_result", "rikishi2_win", "rikishi2_birth_date", "rikishi2_prev",
				"odds1", "odds2"
			)
		)
	)

	
# train & test data sets
all_data %>% 
	historical() %>% 
	filter(is.na(odds1)) %>% 
	drop_extra_cols() %>% 
	write_csv(., "train.csv")

all_data %>% 
	historical() %>% 
	filter(!is.na(odds1)) %>% 
	drop_extra_cols() %>% 
	write_csv(., "test.csv")

all_data %>% 
	historical() %>% 
	filter(!is.na(odds1)) %>% 
	write_csv(., "test2.csv")
