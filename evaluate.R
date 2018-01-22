library(ROCR)
library(tidyverse)


source("common.R")


all_data <- read_csv("all_data.csv", guess_max = 1e+4)

test_data <- all_data %>% 
	filter(
		!is.na(odds1),
		!is.na(rikishi1_win)
	) %>% 
	mutate_at(vars(contains("odds")), as.numeric)





#df <- cbind(
#	read_csv("test2.csv"),
#	read_csv("v2.1_pred.csv")
#)


# Marathonbet
mb_pred <- with(
	test_data,
	prediction(
		predictions = 1 / odds1 / (1 / odds1 + 1 / odds2),
		labels = rikishi1_win
	)
)

unlist(performance(mb_pred, "auc")@y.values)


# my model
glm_fit_pred <- predict(
	readRDS("glm_fit.rds"),
	newdata = test_data %>% 
		drop_extra_cols() %>% 
		select(-rikishi1_win),
	type = "prob"
)

my_pred <- prediction(
	predictions = glm_fit_pred$yes,
	labels = test_data$rikishi1_win
)

unlist(performance(my_pred, "auc")@y.values)


# Amazon ML model
#az_pred <- prediction(
#	predictions = df$score,
#	labels = df$trueLabel
#)

#unlist(performance(az_pred, "auc")@y.values)


# ROC curves
plot(performance(mb_pred, measure = "tpr", x.measure = "fpr"))
plot(performance(my_pred, measure = "tpr", x.measure = "fpr"), add = TRUE, col = "blue")
lines(x = c(0, 1), y = c(0, 1), lty = 2)


# hypothetical expected value
ev_threshold <- .3

df %>% 
	mutate(
		ev = score * odds1 - 1,
		gross = ifelse(
			ev > ev_threshold,
			trueLabel * odds1 - 1,
			0
		)
	) %>% 
	group_by(basho) %>% 
	summarise(
		offers = n(),
		bets = sum(gross != 0),
		gross = sum(gross)
	)

test_data %>% 
	mutate(
		rikishi1_win_prob = glm_fit_pred$yes,
		ev = rikishi1_win_prob * odds1 - 1,
		gross = ifelse(ev > ev_threshold, rikishi1_win * odds1 - 1, 0)
	) %>% View()
	group_by(basho) %>% 
	summarise(
		offers = n(),
		bets = sum(gross != 0),
		gross = sum(gross)
	)
