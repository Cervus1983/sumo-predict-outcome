library(stringr)


# adds information from {banzuke}
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


# adds rank_name & rank_level columns (rank "Y1e" -> rank_name "Y" + rank_level 1)
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


# adds current tournament form ("0-0" on day 1)
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


# adds historical head-to-head wins (excl. walkovers)
add_head_to_head <- function(data) data %>% 
	arrange(basho, day) %>% 
	mutate(
		rikishi1_win_no_fusen = ifelse(kimarite == "fusen" || is.na(rikishi1_win), 0, rikishi1_win),
		rikishi2_win_no_fusen = ifelse(kimarite == "fusen" || is.na(rikishi2_win), 0, rikishi2_win)
	) %>% 
	group_by(rikishi1_id, rikishi2_id) %>% 
	mutate(
		rikishi1_head_to_head_wins = cumsum(rikishi1_win_no_fusen) - rikishi1_win_no_fusen,
		rikishi2_head_to_head_wins = cumsum(rikishi2_win_no_fusen) - rikishi2_win_no_fusen
	) %>% 
	ungroup() %>% 
	select(
		-rikishi1_win_no_fusen,
		-rikishi2_win_no_fusen
	)
