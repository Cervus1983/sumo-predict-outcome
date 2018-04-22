library(tidyverse)

benchmark_pred <- "benchmark.rds" %>% 
	readRDS() %>% 
	mlr::getBMRPredictions()

data <- "data.rds" %>% 
	readRDS() %>% 
	filter(!is_train) %>% 
	transmute(
		basho,
		y = as.integer(y) - 1,
		odds1_open,
		classif.binomial = benchmark_pred[["select(data, -is_train)"]][["classif.binomial"]][["data"]][["prob.yes"]]
	)

ev_threshold <- 1

data %>% 
	mutate(EV = ifelse(classif.binomial * odds1_open > ev_threshold, y * odds1_open - 1, 0)) %>% 
	group_by(basho) %>% 
	summarise(
		offers = n(),
		bets = sum(EV != 0),
		gross = sum(EV)
	)
