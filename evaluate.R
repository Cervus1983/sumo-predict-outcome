library(ROCR)


source("common.R")


all_data <- readRDS("all_data.rds")
test_data <- all_data %>% filter(!is.na(rikishi1_win), !is.na(odds1))


# Marathonbet
mb_pred <- with(
	test_data,
	prediction(
		predictions = 1 / odds1 / (1 / odds1 + 1 / odds2),
		labels = rikishi1_win
	)
)

unlist(performance(mb_pred, "auc")@y.values)
# 0.623


# my model
load("glm_fit.Rdata")

glm_fit_pred <- predict(
	glm_fit,
	newdata = test_data,
	type = "prob"
)

my_pred <- prediction(
	predictions = glm_fit_pred$yes,
	labels = test_data$rikishi1_win
)

unlist(performance(my_pred, "auc")@y.values)
# 0.638


# ROC curves
plot(performance(mb_pred, measure = "tpr", x.measure = "fpr"))
plot(performance(my_pred, measure = "tpr", x.measure = "fpr"), add = TRUE, col = "blue")
lines(x = c(0, 1), y = c(0, 1), lty = 2)


# PROFIT
test_data %>% 
	mutate(
		rikishi1_win_prob = glm_fit_pred$yes,
		ev = rikishi1_win_prob * odds1 - 1,
		gross = ifelse(ev > .3, rikishi1_win * odds1 - 1, 0)
	) %>% 
	group_by(basho) %>% 
	summarise(
		offers = n(),
		bets = sum(gross != 0),
		gross = sum(gross)
	)
