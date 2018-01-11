library(ROCR)
library(tidyverse)

load("sumo.RData")

marathonbet <- inner_join(odds, results) %>% 
	mutate(
		rikishi1_win_prob = 1 / odds1 / (1 / odds1 + 1 / odds2),
		rikishi2_win_prob = 1 / odds2 / (1 / odds1 + 1 / odds2)
	)

make_predictions <- function(data, model) data %>% 
	cbind(., predict(model, ., type = "prob")) %>% 
	mutate(
		rikishi1_win_prob = yes,
		rikishi2_win_prob = no
	) %>% 
	select(-yes, -no)

compare_ROC <- function(data, model) {
	mb_pred <- prediction(
		predictions = marathonbet$rikishi1_win_prob,
		labels = marathonbet$rikishi1_win
	)
	
	mb_auc <- unlist(performance(mb_pred, "auc")@y.values)
	
	mine <- make_predictions(data, model)
	
	my_pred <- prediction(
		predictions = mine$rikishi1_win_prob,
		labels = mine$rikishi1_win
	)
	
	my_auc <- unlist(performance(my_pred, "auc")@y.values)
	
	# ROC curve	(AUC in the title)
	plot(
		performance(mb_pred, measure = "tpr", x.measure = "fpr"),
		col = "red",
		main = sprintf(
			"%.1f%% %s %.1f%%",
			my_auc * 100,
			switch(sign(my_auc - mb_auc) + 2, "<", "=", ">"),
			mb_auc * 100
		)
	)
	
	plot(
		performance(my_pred, measure = "tpr", x.measure = "fpr"),
		add = TRUE
	)
	
	lines(x = c(0, 1), y = c(0, 1), lty = 2)
}
	
simulate_betting <- function(data, model, EV_threshold = 0) data %>% 
	make_predictions(model) %>% 
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
