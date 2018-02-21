library(pROC)
library(tidyverse)


all_ranks <- c("Y", "O", "S", "K", "M", "Bg", "J", "Ms", "Sd", "Jd", "Jk")


# switches "1" <-> "2" columns of {data}
# if {by} is provided, only switches columns when {by1} > {by2}
switch_columns <- function(data, by = NA) {
	df <- apply(data, 1, function(x) {
		tmp <- names(x)
		
		if (is.na(by) || x[[paste0(by, "1")]] > x[[paste0(by, "2")]]) {
			tmp[grep("1", names(x))] <- names(x)[grep("2", names(x))]
			tmp[grep("2", names(x))] <- names(x)[grep("1", names(x))]
		}
		
		set_names(x[tmp], NULL)
	}) %>% 
		t() %>% 
		as.tibble() %>% 
		set_names(names(data))
	
	df[] <- mapply(
		FUN = as,
		df,
		sapply(data, class),
		SIMPLIFY = FALSE
	)
	
	df
}


# removes records unused in training & evaluation
historical <- function(data) data %>% 
	filter(
		basho > 1989, # remove first 6 years
		kimarite != "fusen", # remove walkovers
		!is.na(rikishi1_win) # remove upcoming bouts
	)


# removes bouts outside makuuchi (top division)
makuuchi <- function(data) data %>% 
	filter(rikishi1_rank_name < "J" | rikishi2_rank_name < "J")


# models' predictors
glm_predictors <- c(
	"rikishi1_height", "rikishi1_weight",
	"rikishi1_prev_w", "rikishi1_prev_l",
	"rikishi2_height", "rikishi2_weight",
	"rikishi2_prev_w", "rikishi2_prev_l",
	"rikishi1_age", "rikishi2_age",
	"rikishi1_rank_name", "rikishi1_rank_level",
	"rikishi1_rank_name_prev", "rikishi1_rank_level_prev",
	"rikishi2_rank_name", "rikishi2_rank_level",
	"rikishi2_rank_name_prev", "rikishi2_rank_level_prev",
	"rikishi1_form", "rikishi2_form",
	"rikishi1_head_to_head_wins", "rikishi2_head_to_head_wins"
)

xgboost_predictors <- c(
	"rikishi1_height", "rikishi1_weight",
	"rikishi1_prev_w", "rikishi1_prev_l",
	"rikishi2_height", "rikishi2_weight",
	"rikishi2_prev_w", "rikishi2_prev_l",
	"rikishi1_age", "rikishi2_age",
	"rikishi1_rank_name", "rikishi1_rank_level",
	"rikishi1_rank_name_prev", "rikishi1_rank_level_prev",
	"rikishi2_rank_name", "rikishi2_rank_level", "rikishi2_rank_name_prev",
	"rikishi2_rank_level_prev",
	"rank_vs_rank",
	"rikishi1_form", "rikishi2_form",
	"rikishi1_wins_before", "rikishi2_wins_before",
	"rikishi1_win_rate_before", "rikishi2_win_rate_before",
	"rikishi1_win_rate_needed", "rikishi2_win_rate_needed",
	"rikishi1_head_to_head_wins", "rikishi2_head_to_head_wins",
	"rikishi1_head_to_head_win_rate"
)
