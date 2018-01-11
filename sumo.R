source("common.R")
source("sumo_evaluate.R")


load("model1.RData")

compare_ROC(df_test, model1)
simulate_betting(df_test, model1, .3)


load("model2.RData")

compare_ROC(df_test, model2)
simulate_betting(df_test, model2, .2)


# predictions
make_predictions(df_test, model2) %>% View()


# single prediction
predict(
	model2,
	tibble(
		day = 1,
		rikishi1_rank_name = "Y",
		rikishi2_rank_name = "Y",
		rikishi1_rank_level = 1,
		rikishi2_rank_level = 1,
		rikishi1_win_prc = .5,
		rikishi2_win_prc = .5,
		rikishi1_fusen = 0,
		rikishi2_fusen = 0,
		rikishi1_hth_wins = 12,
		rikishi2_hth_wins = 12
	),
	type = "prob"
)


# ranks & head-to-head history
expand.grid(
	day = 1,
	rikishi1_rank_name = "M",
	rikishi2_rank_name = "M",
	rikishi1_rank_level = seq(2, 14, by = 2),
	rikishi2_rank_level = 8,
	rikishi1_win_prc = .5,
	rikishi2_win_prc = .5,
	rikishi1_fusen = 0,
	rikishi2_fusen = 0,
	rikishi1_hth_wins = 0:10,
	rikishi2_hth_wins = 5
) %>% 
	make_predictions(model2) %>% 
	ggplot() +
		geom_point(aes(
			rikishi1_hth_wins,
			rikishi1_win_prob,
			colour = as.factor(rikishi1_rank_level)
		)) +
		theme_minimal() +
		theme(legend.title = element_blank())
