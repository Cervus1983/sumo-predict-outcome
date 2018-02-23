library(mlr)
library(tidyverse)



data <- "data.rds" %>% 
	readRDS() %>% 
	select(
		# predictors
		rikishi1_height, rikishi1_weight,
		rikishi1_prev_w, rikishi1_prev_l,
		rikishi2_height, rikishi2_weight,
		rikishi2_prev_w, rikishi2_prev_l,
		rikishi1_age, rikishi2_age,
		rikishi1_rank_name, rikishi1_rank_level,
		rikishi1_rank_name_prev, rikishi1_rank_level_prev,
		rikishi2_rank_name, rikishi2_rank_level, rikishi2_rank_name_prev,
		rikishi2_rank_level_prev,
		rank_vs_rank,
		rikishi1_form, rikishi2_form,
		rikishi1_wins_before, rikishi2_wins_before,
		rikishi1_win_rate_before, rikishi2_win_rate_before,
		rikishi1_win_rate_needed, rikishi2_win_rate_needed,
		rikishi1_head_to_head_wins, rikishi2_head_to_head_wins,
		rikishi1_head_to_head_win_rate,
		# train / test
		is_train,
		#target
		y
	) %>% 
	mutate_if(is.ordered, as.integer)

task <- makeClassifTask(
	id = "train",
	data = select(data, -is_train),
	target = "y",
	positive = "yes"
)

learner <- makeFeatSelWrapper(
	makeLearner("classif.binomial", predict.type = "prob", fix.factors.prediction = TRUE),
	resampling = makeResampleDesc("CV", iters = 3),
	measures = auc,
	control = makeFeatSelControlRandom(maxit = 10)
)

model <- train(learner, task, subset = data$is_train)






# General Linear Model
data <- "data.rds" %>% 
	readRDS() %>% 
	select(
		# predictors
		rikishi1_height, rikishi1_weight,
		rikishi1_prev_w, rikishi1_prev_l,
		rikishi2_height, rikishi2_weight,
		rikishi2_prev_w, rikishi2_prev_l,
		rikishi1_age, rikishi2_age,
		rikishi1_rank_name, rikishi1_rank_level,
		rikishi1_rank_name_prev, rikishi1_rank_level_prev,
		rikishi2_rank_name, rikishi2_rank_level,
		rikishi2_rank_name_prev, rikishi2_rank_level_prev,
		rikishi1_form, rikishi2_form,
		rikishi1_head_to_head_wins, rikishi2_head_to_head_wins,
		# train / test
		is_train,
		#target
		y
	) %>% 
	mutate_if(is.ordered, as.integer)

task <- makeClassifTask(
	id = "sumo",
	data = select(data, -is_train),
	target = "y",
	positive = "yes"
)

fv <- generateFilterValuesData(task, method = c("chi.squared", "information.gain"))
plotFilterValues(fv)

learner <- makeLearner("classif.binomial", predict.type = "prob")

model <- train(learner, task, subset = data$is_train)

predictions <- predict(model, task, subset = !data$is_train)

performance(predictions, auc)
calculateConfusionMatrix(predictions)
calculateROCMeasures(predictions)

predictions %>% 
	generateThreshVsPerfData(measures = list(fpr, tpr)) %>% 
	plotROCCurves()


# eXtreme Gradient Boosting
data <- "data.rds" %>% 
	readRDS() %>% 
	select(
		# predictors
		rikishi1_height, rikishi1_weight,
		rikishi1_prev_w, rikishi1_prev_l,
		rikishi2_height, rikishi2_weight,
		rikishi2_prev_w, rikishi2_prev_l,
		rikishi1_age, rikishi2_age,
		rikishi1_rank_name, rikishi1_rank_level,
		rikishi1_rank_name_prev, rikishi1_rank_level_prev,
		rikishi2_rank_name, rikishi2_rank_level, rikishi2_rank_name_prev,
		rikishi2_rank_level_prev,
		rank_vs_rank,
		rikishi1_form, rikishi2_form,
		rikishi1_wins_before, rikishi2_wins_before,
		rikishi1_win_rate_before, rikishi2_win_rate_before,
		rikishi1_win_rate_needed, rikishi2_win_rate_needed,
		rikishi1_head_to_head_wins, rikishi2_head_to_head_wins,
		rikishi1_head_to_head_win_rate,
		# train / test
		is_train,
		#target
		y
	)

data <- createDummyFeatures(
	data,
	target = "y",
	cols = data %>% 
		select(-y) %>% 
		select_if(is.factor) %>% 
		names()
)

task <- makeClassifTask(
	id = "train",
	data = select(data, -is_train),
	target = "y",
	positive = "yes"
)

learner <- makeLearner(
	"classif.xgboost",
	predict.type = "prob",
	par.vals = list(eval_metric = "auc")
)

model <- train(learner, task, subset = data$is_train)

predictions <- predict(model, task, subset = !data$is_train)

performance(predictions, auc)
calculateConfusionMatrix(predictions)
calculateROCMeasures(predictions)

predictions %>% 
	generateThreshVsPerfData(measures = list(fpr, tpr)) %>% 
	plotROCCurves()


# eXtreme Gradient Boosting with tuned hyperparameters
tuned_params <- tuneParams(
	learner,
	task,
	resampling = makeResampleDesc("CV", iters = 4),
	measures = auc,
	par.set = makeParamSet(
		makeNumericParam("eta", lower = .1, upper = .5),
		makeIntegerParam("max_depth", lower = 2, upper = 8),
		makeIntegerParam("nrounds", lower = 50, upper = 200),
		makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
	),
	control = makeTuneControlRandom(maxit = 100)
)
# Result: nrounds=149; max_depth=3; eta=0.118; lambda=0.179 : auc.test.mean=0.661
# Result: eta=0.117; max_depth=4; nrounds=200; lambda=0.99 : auc.test.mean=0.66
saveRDS(tuned_params, "classif.xgboost/tuned_params.rds")

tuned_learner <- setHyperPars(learner, par.vals = tuned_params$x)

model <- train(tuned_learner, task, subset = data$is_train)

predictions <- predict(model, task, subset = !data$is_train)

performance(predictions, auc)
calculateConfusionMatrix(predictions)
calculateROCMeasures(predictions)

predictions %>% 
	generateThreshVsPerfData(measures = list(fpr, tpr)) %>% 
	plotROCCurves()


# benchmark
learners <- list(
	makeLearner(
		"classif.binomial",
		predict.type = "prob"
	),
	makeLearner(
		"classif.xgboost",
		predict.type = "prob",
		par.vals = list(eval_metric = "auc")
	) %>% setHyperPars(par.vals = tuned_params$x)
)

# could benchmark across basho?
b <- benchmark(
	learners,
	task,
	resamplings = makeFixedHoldoutInstance(
		train.inds = which(data$is_train),
		test.inds = which(!data$is_train),
		size = nrow(data)
	),
	measures = auc
)

saveRDS(b, "benchmark.rds")

b %>% 
	generateThreshVsPerfData(measures = list(fpr, tpr)) %>% 
	plotROCCurves()
