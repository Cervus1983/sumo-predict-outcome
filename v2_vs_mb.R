mb <- all_data %>% 
	filter(!is.na(odds1)) %>% 
	mutate(
		rikishi1_win_prob = 1 / odds1 / (1 / odds1 + 1 / odds2),
		rikishi2_win_prob = 1 / odds2 / (1 / odds1 + 1 / odds2)
	)

mb_pred <- prediction(
	predictions = mb$rikishi1_win_prob,
	labels = mb$rikishi1_win
)

unlist(performance(mb_pred, "auc")@y.values)



az_pred <- prediction(
	predictions = read_csv("v2_pred.csv")$score,
	labels = read_csv("v2_pred.csv")$trueLabel
)

unlist(performance(az_pred, "auc")@y.values)



plot(performance(mb_pred, measure = "tpr", x.measure = "fpr"))
plot(performance(az_pred, measure = "tpr", x.measure = "fpr"), add = TRUE, col = "blue")
