library(tidyverse)

benchmark_data <- readRDS("benchmark.rds")
benchmark_pred <- mlr::getBMRPredictions(benchmark_data)

data <- "data.rds" %>% 
	readRDS() %>% 
	filter(!is_train) %>% 
	transmute(
		basho,
		y = as.integer(y) - 1,
		odds1_open,
		classif.binomial = benchmark_pred[["train"]][["classif.binomial"]][["data"]][["prob.yes"]],
		classif.xgboost = benchmark_pred[["train"]][["classif.xgboost"]][["data"]][["prob.yes"]]
	)

ev_threshold <- 1.2

data %>% 
	mutate(
		glm = ifelse(classif.binomial * odds1_open > ev_threshold, y * odds1_open - 1, 0),
		xgboost = ifelse(classif.xgboost * odds1_open > ev_threshold, y * odds1_open - 1, 0),
		both = ifelse(classif.binomial * odds1_open > ev_threshold & classif.xgboost * odds1_open > ev_threshold, y * odds1_open - 1, 0)
	) %>% 
	group_by(basho) %>% 
	summarise(
		offers = n(),
		
		glm_bets = sum(glm != 0),
		glm_gross = sum(glm),
		
		xgboost_bets = sum(xgboost != 0),
		xgboost_gross = sum(xgboost),
		
		both_bets = sum(both != 0),
		both_gross = sum(both)
	) %>% 
	View()
