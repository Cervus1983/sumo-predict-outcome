library(tidyverse)

# data
banzuke <- read_csv("banzuke.csv")
results <- read_csv("results.csv")
odds <- read_csv("odds.csv")

# functions
sapply(list.files("v2_functions", full.names = TRUE), source)

# rich data set
all_data <- results %>% 
	add_banzuke_info() %>% 
	add_current_basho_performance() %>% 
	add_head_to_head() %>% 
	filter(basho > 2007) %>% 
	left_join(., odds)

# train & test data sets
data_to_model <- function(data) data %>% 
	# remove walkovers
	filter(kimarite != "fusen") %>% 
	# recode "win" columns (0 -> no, 1 -> yes)
	mutate_at(
		vars(starts_with("rikishi1_win"), starts_with("rikishi2_win")),
		function(x) recode(x + 1, "no", "yes")
	) %>% 
	# recode "kimarite_day_minus_N" columns (fusen / not fusen)
	mutate_at(
		vars(starts_with("rikishi1_kimarite"), starts_with("rikishi2_kimarite")),
		function(x) ifelse(x == "fusen", "fusen", "not fusen")
	) %>% 
	# birth date -> YOB
	mutate(
		rikishi1_yob = as.integer(str_sub(rikishi1_birth_date, -4, -1)),
		rikishi2_yob = as.integer(str_sub(rikishi2_birth_date, -4, -1))
	) %>% 
	select(-one_of(c(
		"basho",
		"rikishi1_id", "rikishi1_shikona", "rikishi1_result", "rikishi1_birth_date",
		"kimarite",
		"rikishi2_id", "rikishi2_shikona", "rikishi2_result", "rikishi2_win", "rikishi2_birth_date",
		"odds1", "odds2"
	)))

all_data %>% 
	filter(is.na(odds1)) %>% 
	data_to_model() %>% 
	write_csv(., "train.csv")

all_data %>% 
	filter(!is.na(odds1)) %>% 
	data_to_model() %>% 
	write_csv(., "test.csv")
