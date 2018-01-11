library(stringr)
library(tidyverse)

source("common.R")

# parses rank (Y1e) into rank_name (Y) + rank_level (1)
parse_rank <- function(data) data %>% 
	mutate(
		rikishi1_rank_name = ordered(str_extract(rikishi1_rank, "^\\D+"), levels = all_ranks),
		rikishi1_rank_level = as.integer(str_extract(rikishi1_rank, "\\d+")),
		rikishi2_rank_name = ordered(str_extract(rikishi2_rank, "^\\D+"), levels = all_ranks),
		rikishi2_rank_level = as.integer(str_extract(rikishi2_rank, "\\d+"))
	)

# adds # of wins (inlc. walkovers), losses, and forfeits before the bout
add_current_tournament <- function(data) data %>% 
	mutate(
		rikishi1_won = as.integer(str_match(rikishi1_result, "^(\\d+)-")[, 2]) - rikishi1_win,
		rikishi1_lost = as.integer(str_match(rikishi1_result, "^\\d+-(\\d+)")[, 2]) - rikishi2_win,
		rikishi1_fusen = as.integer(str_match(rikishi1_result, "^\\d+-\\d+-(\\d+)")[, 2]),
		rikishi2_won = as.integer(str_match(rikishi2_result, "^(\\d+)-")[, 2]) - rikishi2_win,
		rikishi2_lost = as.integer(str_match(rikishi2_result, "^\\d+-(\\d+)")[, 2]) - rikishi1_win,
		rikishi2_fusen = as.integer(str_match(rikishi2_result, "^\\d+-\\d+-(\\d+)")[, 2])
	) %>% 
	replace_na(list(rikishi1_fusen = 0, rikishi2_fusen = 0))

# adds # of head-to-head wins (excl. walkovers)
add_hth_wins <- function(data) data %>% 
	arrange(basho, day) %>% 
	mutate(
		rikishi1_win_no_fusen = ifelse(kimarite == "fusen", 0, rikishi1_win),
		rikishi2_win_no_fusen = ifelse(kimarite == "fusen", 0, rikishi2_win)
	) %>% 
	group_by(rikishi1_id, rikishi2_id) %>% 
	mutate(
		rikishi1_hth_wins = cumsum(rikishi1_win_no_fusen) - rikishi1_win_no_fusen,
		rikishi2_hth_wins = cumsum(rikishi2_win_no_fusen) - rikishi2_win_no_fusen
	) %>% 
	ungroup() %>% 
	select(
		-rikishi1_win_no_fusen,
		-rikishi2_win_no_fusen
	)
