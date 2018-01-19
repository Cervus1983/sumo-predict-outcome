library(ROCR)
library(tidyverse)


source("common.R")


df <- cbind(
	read_csv("test2.csv"),
	read_csv("v2.1_pred.csv")
)


# Marathonbet
mb_pred <- prediction(
	predictions = with(df, 1 / odds1 / (1 / odds1 + 1 / odds2)),
	labels = with(df, trueLabel)
)

unlist(performance(mb_pred, "auc")@y.values)


# Amazon ML model
az_pred <- prediction(
	predictions = df$score,
	labels = df$trueLabel
)

unlist(performance(az_pred, "auc")@y.values)


# ROC curves
plot(performance(mb_pred, measure = "tpr", x.measure = "fpr"))
plot(performance(az_pred, measure = "tpr", x.measure = "fpr"), add = TRUE, col = "blue")
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
