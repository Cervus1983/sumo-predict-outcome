library(stringr)
library(tidyverse)


# information from {banzuke}
add_banzuke_info <- function(data, banzuke) {
	banzuke_relevant_cols <- c("basho", "id", "birth_date", "height", "weight", "prev", "prev_w", "prev_l")

	data %>% 
		inner_join(
			.,
			banzuke %>% 
				select(one_of(banzuke_relevant_cols)) %>% 
				set_names(c("basho", paste("rikishi1", tail(names(.), -1), sep = "_")))
		) %>% 
		inner_join(
			.,
			banzuke %>% 
				select(one_of(banzuke_relevant_cols)) %>% 
				set_names(c("basho", paste("rikishi2", tail(names(.), -1), sep = "_")))
		)
}


# birth date -> age (in years, on the 1st of the month)
add_age <- function(data) data %>% 
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
	)


# rank_name & rank_level columns (rank "Y1e" -> rank_name "Y" + rank_level 1)
all_ranks <- c("Y", "O", "S", "K", "M", "Bg", "J", "Ms", "Sd", "Jd", "Jk")

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
	) %>% 
	replace_na(list(
		rikishi1_rank_level_prev = 0,
		rikishi2_rank_level_prev = 0
	))


# current tournament form ("0-0" on day 1), also split into wins/losses/misses
# TO DO: take fusens into account
add_form <- function(data) data %>% 
	group_by(basho, rikishi1_id) %>% 
	mutate(
		rikishi1_form = lag(rikishi1_result),
		rikishi1_form = ifelse(is.na(rikishi1_form), "0-0", sub("\\(.*\\)", "", rikishi1_form)),
		rikishi1_w = as.integer(str_match(rikishi1_form, "^(\\d+)")[, 2]),
		rikishi1_l = as.integer(str_match(rikishi1_form, "^\\d+-(\\d+)")[, 2]),
		rikishi1_m = as.integer(str_match(rikishi1_form, "^\\d+-\\d+-(\\d+)")[, 2])
	) %>% 
	group_by(basho, rikishi2_id) %>% 
	mutate(
		rikishi2_form = lag(rikishi2_result),
		rikishi2_form = ifelse(is.na(rikishi2_form), "0-0", sub("\\(.*\\)", "", rikishi2_form)),
		rikishi2_w = as.integer(str_match(rikishi2_form, "^(\\d+)")[, 2]),
		rikishi2_l = as.integer(str_match(rikishi2_form, "^\\d+-(\\d+)")[, 2]),
		rikishi2_m = as.integer(str_match(rikishi2_form, "^\\d+-\\d+-(\\d+)")[, 2])
	) %>% 
	ungroup() %>% 
	replace_na(replace = list(rikishi1_m = 0, rikishi2_m = 0))


# https://en.wiktionary.org/wiki/kachi-koshi
add_kachi_koshi <- function(data) data %>% 
	mutate(
		rikishi1_kachi_koshi = ifelse(
			rikishi1_w >= 8,
			"yes",
			ifelse(8 - rikishi1_w > 16 - day, "no", "maybe")
		),
		rikishi2_kachi_koshi = ifelse(
			rikishi2_w >= 8,
			"yes",
			ifelse(8 - rikishi2_w > 16 - day, "no", "maybe")
		)
	)


# winning & losing streaks
add_streak <- function(data) data %>% 
	group_by(basho, rikishi1_id) %>% 
	mutate(
		# https://stackoverflow.com/a/48552843/17216
		rikishi1_w_streak = ave(rikishi1_win == 1, cumsum(rikishi1_win == 0), FUN = cumsum),
		rikishi1_w_streak = lag(rikishi1_w_streak),
		rikishi1_l_streak = ave(rikishi1_win == 0, cumsum(rikishi1_win == 1), FUN = cumsum),
		rikishi1_l_streak = lag(rikishi1_l_streak)
	) %>% 
	group_by(basho, rikishi2_id) %>% 
	mutate(
		rikishi2_w_streak = ave(rikishi2_win == 1, cumsum(rikishi2_win == 0), FUN = cumsum),
		rikishi2_w_streak = lag(rikishi2_w_streak),
		rikishi2_l_streak = ave(rikishi2_win == 0, cumsum(rikishi2_win == 1), FUN = cumsum),
		rikishi2_l_streak = lag(rikishi2_l_streak)
	) %>% 
	ungroup() %>% 
	replace_na(replace = list(rikishi1_w_streak = 0, rikishi1_l_streak = 0, rikishi2_w_streak = 0, rikishi2_l_streak = 0))


# head-to-head wins, streaks & win rate (excl. walkovers)
add_head_to_head <- function(data) data %>% 
	mutate(
		rikishi1_win_no_fusen = ifelse(kimarite %in% c(NA, "fusen"), 0, rikishi1_win),
		rikishi2_win_no_fusen = ifelse(kimarite %in% c(NA, "fusen"), 0, rikishi2_win)
	) %>% 
	group_by(rikishi1_id, rikishi2_id) %>% 
	mutate(
		rikishi1_head_to_head_wins = cumsum(rikishi1_win_no_fusen) - rikishi1_win_no_fusen,
		rikishi2_head_to_head_wins = cumsum(rikishi2_win_no_fusen) - rikishi2_win_no_fusen,
		rikishi1_head_to_head_w_streak = ave(kimarite != "fusen" & rikishi1_win == 1, cumsum(kimarite != "fusen" & rikishi1_win == 0), FUN = cumsum),
		rikishi1_head_to_head_w_streak = lag(rikishi1_head_to_head_w_streak),
		rikishi2_head_to_head_w_streak = ave(kimarite != "fusen" & rikishi2_win == 1, cumsum(kimarite != "fusen" & rikishi2_win == 0), FUN = cumsum),
		rikishi2_head_to_head_w_streak = lag(rikishi2_head_to_head_w_streak)
	) %>% 
	ungroup() %>% 
	select(
		-rikishi1_win_no_fusen,
		-rikishi2_win_no_fusen
	) %>% 
	replace_na(replace = list(rikishi1_head_to_head_w_streak = 0, rikishi2_head_to_head_w_streak = 0)) %>% 
	mutate(
		rikishi1_head_to_head_win_rate = ifelse(
			rikishi1_head_to_head_wins + rikishi2_head_to_head_wins > 0,
			rikishi1_head_to_head_wins / (rikishi1_head_to_head_wins + rikishi2_head_to_head_wins),
			.5
		)
	)
