marathonbet <- inner_join(odds, results) %>% 
	mutate(
		rikishi1_win_prob = 1 / odds1 / (1 / odds1 + 1 / odds2),
		rikishi2_win_prob = 1 / odds2 / (1 / odds1 + 1 / odds2)
	)

mine <- make_predictions(df_test, model1)

amazon <- df_test %>% 
	cbind(read_csv("C:/Users/mikzhi/Desktop/bp-VgCfjFAvYgi-sumo_model2_test.csv")) %>% 
	mutate(
		rikishi1_win_prob = score,
		rikishi2_win_prob = 1 - score
	)

mb_pred <- prediction(
	predictions = marathonbet$rikishi1_win_prob,
	 labels = marathonbet$rikishi1_win
)

mb_auc <- unlist(performance(mb_pred, "auc")@y.values)

my_pred <- prediction(
	predictions = mine$rikishi1_win_prob,
	labels = mine$rikishi1_win
)

my_auc <- unlist(performance(my_pred, "auc")@y.values)

amazon_pred <- prediction(
	predictions = amazon$rikishi1_win_prob,
	labels = amazon$rikishi1_win
)

amazon_auc <- unlist(performance(amazon_pred, "auc")@y.values)

# ROC curve	(AUC in the title)
plot(
	performance(mb_pred, measure = "tpr", x.measure = "fpr"),
	col = "red",
	#main = sprintf(
	#	"%.1f%% %s %.1f%%",
	#	my_auc * 100,
	#	switch(sign(my_auc - mb_auc) + 2, "<", "=", ">"),
	#	mb_auc * 100
	#)
)

plot(
	performance(my_pred, measure = "tpr", x.measure = "fpr"),
	add = TRUE
)

plot(
	performance(amazon_pred, measure = "tpr", x.measure = "fpr"),
	col = "blue",
	add = TRUE
)

lines(x = c(0, 1),
	  y = c(0, 1),
	  lty = 2)

amazon %>% 
		mutate(
			EV1 = rikishi1_win_prob * odds1 - 1,
			EV2 = (1 - rikishi1_win_prob) * odds2 - 1,
			gross = ifelse(
				EV1 > EV_threshold,
				rikishi1_win * odds1 - 1,
				ifelse(EV2 > EV_threshold, rikishi2_win * odds2 - 1, 0)
			)
		) %>% 
		group_by(basho) %>% 
		summarise(
			offers = n(),
			bets = sum(gross != 0),
			gross = sum(gross)
		)

