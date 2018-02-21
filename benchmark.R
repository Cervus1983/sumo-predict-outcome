library(caret)
library(Matrix)
library(ROCR)
library(xgboost)


source("common.R")


# models
load("models/glm.Rdata")
xgboost_fit <- xgb.load("models/xgboost.model")


# test data (priced bouts)
test_data <- "all_data.rds" %>% 
	readRDS() %>% 
	filter(!is.na(odds1_open))


# add predictions & save
test_data %>% 
	mutate(
		# MarathonBet
		mb_open = 1 / odds1_open / (1 / odds1_open + 1 / odds2_open),
		mb_close = 1 / odds1_close / (1 / odds1_close + 1 / odds2_close),
		# General Linear Model
		glm = predict(glm_fit, test_data, "prob")$yes,
		# eXtreme Gradient Boosting
		xgboost = predict(
			xgboost_fit,
			sparse.model.matrix(
				rikishi1_win ~ . - 1,
				data = test_data %>% select(one_of(xgboost_predictors))
			)
		)
	) %>% 
	saveRDS("benchmark_data.rds")


# simulation
ev_threshold <- 1.1

readRDS("benchmark_data.rds") %>% 
	mutate(
		glm = ifelse(glm * odds1_open > ev_threshold, rikishi1_win * odds1_open - 1, 0),
		xgboost = ifelse(xgboost * odds1_open > ev_threshold, rikishi1_win * odds1_open - 1, 0)
	) %>% 
	group_by(basho) %>% 
	summarise(
		offers = n(),
		
		glm_bets = sum(glm != 0),
		glm_gross = sum(glm),
		
		xgboost_bets = sum(xgboost != 0),
		xgboost_gross = sum(xgboost)
	)
