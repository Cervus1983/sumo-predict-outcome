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

# optimal EV threshold
data <- "data.rds" %>% 
	readRDS() %>% 
	filter(!is_train) %>% 
	mutate(rikishi1_win_prob = predict(model, task, subset = !data$is_train)[["data"]][["prob.yes"]])

do.call(
	rbind,
	lapply(
		1 + 0:10 / 10,
		function(ev_threshold) data %>% 
			mutate(gross = ifelse(rikishi1_win_prob * odds1_open > ev_threshold, rikishi1_win * odds1_open - 1, 0)) %>% 
			summarise(
				ev_threshold,
				bets = sum(gross != 0),
				gross = sum(gross)
			)
	)
)
