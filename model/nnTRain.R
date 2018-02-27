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

data <- createDummyFeatures(
	data,
	target = "y",
	cols = data %>% 
		select(-y) %>% 
		select_if(is.factor) %>% 
		names()
)

# model
learner <- makeLearner(
	"classif.nnTrain",
	predict.type = "prob",
	par.vals = list(
		hidden = 400,
		max.number.of.layers = 10,
		numepochs = 100,
		hidden_dropout = .25,
		visible_dropout = .25
	)
)

task <- makeClassifTask(
	data = select(data, -is_train),
	target = "y",
	positive = "yes"
)

model <- train(
	learner,
	task,
	subset = data$is_train
)

# evaluation
model %>% 
	predict(task, subset = !data$is_train) %>% 
	performance(auc)
