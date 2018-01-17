add_head_to_head <- function(data) data %>% 
	arrange(basho, day) %>% 
	mutate(
		rikishi1_win_no_fusen = ifelse(kimarite == "fusen", 0, rikishi1_win),
		rikishi2_win_no_fusen = ifelse(kimarite == "fusen", 0, rikishi2_win)
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
