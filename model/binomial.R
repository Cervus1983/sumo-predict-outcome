library(mlr)
library(tidyverse)


# data
data <- "data.rds" %>% 
	readRDS() %>% 
	select(
		-basho,
		-rikishi1_id,
		-rikishi1_rank,
		-rikishi1_shikona,
		-rikishi1_result,
		-rikishi1_win,
		-kimarite,
		-rikishi2_id,
		-rikishi2_rank,
		-rikishi2_shikona,
		-rikishi2_result,
		-rikishi2_win,
		-rikishi1_birth_date,
		-rikishi1_prev,
		-rikishi2_birth_date,
		-rikishi2_prev,
		-rikishi1_form,
		-rikishi2_form,
		-odds1_open,
		-odds2_open,
		-odds1_close,
		-odds2_close
	) %>% 
	mutate_if(is.ordered, as.integer)


# model
learner <- makeLearner("classif.binomial", predict.type = "prob")

task <- makeClassifTask(
	data = select(data, -is_train),
	target = "y",
	positive = "yes"
)

task %>% 
	generateFilterValuesData(method = "information.gain") %>% 
	.[["data"]] %>% 
	View()

model <- train(learner, task)

saveRDS(model, "model/binomial.rds")


# evaluation
model %>% 
	predict(task, subset = !data$is_train) %>% 
	performance(auc) # 0.6309117


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
