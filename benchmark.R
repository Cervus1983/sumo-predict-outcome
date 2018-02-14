library(caret)
library(ROCR)
library(xgboost)


source("common.R")


all_data <- readRDS("all_data.rds")
test_data <- all_data %>% filter(!is.na(rikishi1_win), !is.na(odds1_open))


# Marathonbet
mb_pred_open <- with(
	test_data,
	prediction(
		predictions = 1 / odds1_open / (1 / odds1_open + 1 / odds2_open),
		labels = rikishi1_win
	)
)

unlist(performance(mb_pred_open, "auc")@y.values)
# 0.6099759

mb_pred_close <- with(
	test_data,
	prediction(
		predictions = 1 / odds1_close / (1 / odds1_close + 1 / odds2_close),
		labels = rikishi1_win
	)
)

unlist(performance(mb_pred_close, "auc")@y.values)
# 0.6119651


# glm
load("glm_fit.Rdata")

glm_fit_pred <- predict(
	glm_fit,
	newdata = test_data,
	type = "prob"
)

glm_pred <- prediction(
	predictions = glm_fit_pred$yes,
	labels = test_data$rikishi1_win
)

unlist(performance(glm_pred, "auc")@y.values)
# 0.6284701


# xgboost
xgboost_fit <- xgb.load("xgboost.model")

xgboost_fit_pred <- predict(
	xgboost_fit,
	sparse.model.matrix(
		rikishi1_win ~ . - 1,
		data = test_data %>% 
			select(one_of(c(xgboost_predictors, "rikishi1_win")))
	)
)

xgboost_pred <- prediction(
	predictions = xgboost_fit_pred,
	labels = test_data$rikishi1_win
)

unlist(performance(xgboost_pred, "auc")@y.values)
# 0.5990722


# ROC curves
plot(performance(mb_pred_open, measure = "tpr", x.measure = "fpr"), col = "green")
plot(performance(mb_pred_close, measure = "tpr", x.measure = "fpr"), add = TRUE, col = "red")
plot(performance(glm_pred, measure = "tpr", x.measure = "fpr"), add = TRUE, col = "blue")
plot(performance(xgboost_pred, measure = "tpr", x.measure = "fpr"), add = TRUE, col = "blue", lty = 2)
lines(x = c(0, 1), y = c(0, 1), lty = 2)


# PROFIT
test_data %>% 
	mutate(
		rikishi1_win_prob = glm_fit_pred$yes,
		ev = rikishi1_win_prob * odds1_open - 1,
		gross = ifelse(ev > .2, rikishi1_win * odds1_open - 1, 0)
	) %>% 
	group_by(basho) %>% 
	summarise(
		offers = n(),
		bets = sum(gross != 0),
		gross = sum(gross)
	)
