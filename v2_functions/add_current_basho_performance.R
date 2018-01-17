add_current_basho_performance <- function(data) data %>% 
	group_by(basho, rikishi1_id) %>% 
	mutate(
		rikishi1_win_day_minus_1 = lag(rikishi1_win, order_by = day),
		rikishi1_kimarite_day_minus_1 = lag(kimarite, order_by = day),
		rikishi1_opponent_rank_day_minus_1 = lag(rikishi2_rank, order_by = day),

		rikishi1_win_day_minus_2 = lag(rikishi1_win, n = 2, order_by = day),
		rikishi1_kimarite_day_minus_2 = lag(kimarite, n = 2, order_by = day),
		rikishi1_opponent_rank_day_minus_2 = lag(rikishi2_rank, n = 2, order_by = day),
		
		rikishi1_win_day_minus_3 = lag(rikishi1_win, n = 3, order_by = day),
		rikishi1_kimarite_day_minus_3 = lag(kimarite, n = 3, order_by = day),
		rikishi1_opponent_rank_day_minus_3 = lag(rikishi2_rank, n = 3, order_by = day),
		
		rikishi1_win_day_minus_4 = lag(rikishi1_win, n = 4, order_by = day),
		rikishi1_kimarite_day_minus_4 = lag(kimarite, n = 4, order_by = day),
		rikishi1_opponent_rank_day_minus_4 = lag(rikishi2_rank, n = 4, order_by = day),
		
		rikishi1_win_day_minus_5 = lag(rikishi1_win, n = 5, order_by = day),
		rikishi1_kimarite_day_minus_5 = lag(kimarite, n = 5, order_by = day),
		rikishi1_opponent_rank_day_minus_5 = lag(rikishi2_rank, n = 5, order_by = day),
		
		rikishi1_win_day_minus_6 = lag(rikishi1_win, n = 6, order_by = day),
		rikishi1_kimarite_day_minus_6 = lag(kimarite, n = 6, order_by = day),
		rikishi1_opponent_rank_day_minus_6 = lag(rikishi2_rank, n = 6, order_by = day),
		
		rikishi1_win_day_minus_7 = lag(rikishi1_win, n = 7, order_by = day),
		rikishi1_kimarite_day_minus_7 = lag(kimarite, n = 7, order_by = day),
		rikishi1_opponent_rank_day_minus_7 = lag(rikishi2_rank, n = 7, order_by = day),
		
		rikishi1_win_day_minus_8 = lag(rikishi1_win, n = 8, order_by = day),
		rikishi1_kimarite_day_minus_8 = lag(kimarite, n = 8, order_by = day),
		rikishi1_opponent_rank_day_minus_8 = lag(rikishi2_rank, n = 8, order_by = day),
		
		rikishi1_win_day_minus_9 = lag(rikishi1_win, n = 9, order_by = day),
		rikishi1_kimarite_day_minus_9 = lag(kimarite, n = 9, order_by = day),
		rikishi1_opponent_rank_day_minus_9 = lag(rikishi2_rank, n = 9, order_by = day),
		
		rikishi1_win_day_minus_10 = lag(rikishi1_win, n = 10, order_by = day),
		rikishi1_kimarite_day_minus_10 = lag(kimarite, n = 10, order_by = day),
		rikishi1_opponent_rank_day_minus_10 = lag(rikishi2_rank, n = 10, order_by = day),
		
		rikishi1_win_day_minus_11 = lag(rikishi1_win, n = 11, order_by = day),
		rikishi1_kimarite_day_minus_11 = lag(kimarite, n = 11, order_by = day),
		rikishi1_opponent_rank_day_minus_11 = lag(rikishi2_rank, n = 11, order_by = day),
		
		rikishi1_win_day_minus_12 = lag(rikishi1_win, n = 12, order_by = day),
		rikishi1_kimarite_day_minus_12 = lag(kimarite, n = 12, order_by = day),
		rikishi1_opponent_rank_day_minus_12 = lag(rikishi2_rank, n = 12, order_by = day),
		
		rikishi1_win_day_minus_13 = lag(rikishi1_win, n = 13, order_by = day),
		rikishi1_kimarite_day_minus_13 = lag(kimarite, n = 13, order_by = day),
		rikishi1_opponent_rank_day_minus_13 = lag(rikishi2_rank, n = 13, order_by = day),
		
		rikishi1_win_day_minus_14 = lag(rikishi1_win, n = 14, order_by = day),
		rikishi1_kimarite_day_minus_14 = lag(kimarite, n = 14, order_by = day),
		rikishi1_opponent_rank_day_minus_14 = lag(rikishi2_rank, n = 14, order_by = day)
	) %>% 
	group_by(basho, rikishi2_id) %>% 
	mutate(
		rikishi2_win_day_minus_1 = lag(rikishi2_win, order_by = day),
		rikishi2_kimarite_day_minus_1 = lag(kimarite, order_by = day),
		rikishi2_opponent_rank_day_minus_1 = lag(rikishi1_rank, order_by = day),

		rikishi2_win_day_minus_2 = lag(rikishi2_win, n = 2, order_by = day),
		rikishi2_kimarite_day_minus_2 = lag(kimarite, n = 2, order_by = day),
		rikishi2_opponent_rank_day_minus_2 = lag(rikishi1_rank, n = 2, order_by = day),
		
		rikishi2_win_day_minus_3 = lag(rikishi2_win, n = 3, order_by = day),
		rikishi2_kimarite_day_minus_3 = lag(kimarite, n = 3, order_by = day),
		rikishi2_opponent_rank_day_minus_3 = lag(rikishi1_rank, n = 3, order_by = day),
		
		rikishi2_win_day_minus_4 = lag(rikishi2_win, n = 4, order_by = day),
		rikishi2_kimarite_day_minus_4 = lag(kimarite, n = 4, order_by = day),
		rikishi2_opponent_rank_day_minus_4 = lag(rikishi1_rank, n = 4, order_by = day),
		
		rikishi2_win_day_minus_5 = lag(rikishi2_win, n = 5, order_by = day),
		rikishi2_kimarite_day_minus_5 = lag(kimarite, n = 5, order_by = day),
		rikishi2_opponent_rank_day_minus_5 = lag(rikishi1_rank, n = 5, order_by = day),
		
		rikishi2_win_day_minus_6 = lag(rikishi2_win, n = 6, order_by = day),
		rikishi2_kimarite_day_minus_6 = lag(kimarite, n = 6, order_by = day),
		rikishi2_opponent_rank_day_minus_6 = lag(rikishi1_rank, n = 6, order_by = day),
		
		rikishi2_win_day_minus_7 = lag(rikishi2_win, n = 7, order_by = day),
		rikishi2_kimarite_day_minus_7 = lag(kimarite, n = 7, order_by = day),
		rikishi2_opponent_rank_day_minus_7 = lag(rikishi1_rank, n = 7, order_by = day),
		
		rikishi2_win_day_minus_8 = lag(rikishi2_win, n = 8, order_by = day),
		rikishi2_kimarite_day_minus_8 = lag(kimarite, n = 8, order_by = day),
		rikishi2_opponent_rank_day_minus_8 = lag(rikishi1_rank, n = 8, order_by = day),
		
		rikishi2_win_day_minus_9 = lag(rikishi2_win, n = 9, order_by = day),
		rikishi2_kimarite_day_minus_9 = lag(kimarite, n = 9, order_by = day),
		rikishi2_opponent_rank_day_minus_9 = lag(rikishi1_rank, n = 9, order_by = day),
		
		rikishi2_win_day_minus_10 = lag(rikishi2_win, n = 10, order_by = day),
		rikishi2_kimarite_day_minus_10 = lag(kimarite, n = 10, order_by = day),
		rikishi2_opponent_rank_day_minus_10 = lag(rikishi1_rank, n = 10, order_by = day),
		
		rikishi2_win_day_minus_11 = lag(rikishi2_win, n = 11, order_by = day),
		rikishi2_kimarite_day_minus_11 = lag(kimarite, n = 11, order_by = day),
		rikishi2_opponent_rank_day_minus_11 = lag(rikishi1_rank, n = 11, order_by = day),
		
		rikishi2_win_day_minus_12 = lag(rikishi2_win, n = 12, order_by = day),
		rikishi2_kimarite_day_minus_12 = lag(kimarite, n = 12, order_by = day),
		rikishi2_opponent_rank_day_minus_12 = lag(rikishi1_rank, n = 12, order_by = day),
		
		rikishi2_win_day_minus_13 = lag(rikishi2_win, n = 13, order_by = day),
		rikishi2_kimarite_day_minus_13 = lag(kimarite, n = 13, order_by = day),
		rikishi2_opponent_rank_day_minus_13 = lag(rikishi1_rank, n = 13, order_by = day),
		
		rikishi2_win_day_minus_14 = lag(rikishi2_win, n = 14, order_by = day),
		rikishi2_kimarite_day_minus_14 = lag(kimarite, n = 14, order_by = day),
		rikishi2_opponent_rank_day_minus_14 = lag(rikishi1_rank, n = 14, order_by = day)
	) %>% 
	ungroup()
