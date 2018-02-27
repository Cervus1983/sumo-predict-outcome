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

# classification task
task <- makeClassifTask(
	data = select(data, -is_train),
	target = "y",
	positive = "yes"
)

# learner 0: glm
learner0 <- makeLearner("classif.binomial", predict.type = "prob")

# learner 1: glm (feature selection)
learner1 <- makeFeatSelWrapper(
	makeLearner("classif.binomial", predict.type = "prob"),
	resampling = makeResampleDesc("CV", iters = 3),
	measures = auc,
	control = makeFeatSelControlSequential(method = "sfbs")
)

# learner 2: xgboost (hyperparameter tuning)
learner2 <- makeTuneWrapper(
	makeLearner("classif.xgboost", predict.type = "prob"),
	resampling = makeResampleDesc("CV", iters = 3),
	measures = auc,
	par.set = makeParamSet(
		makeNumericParam("eta", lower = .05, upper = .5),
		makeIntegerParam("max_depth", lower = 1, upper = 10),
		makeIntegerParam("nrounds", lower = 10, upper = 200),
		makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
	),
	control = makeTuneControlRandom(maxit = 300)
)

# benchmark
benchmark_results <- benchmark(
	list(learner1, learner2),
	task,
	resamplings = makeFixedHoldoutInstance(
		train.inds = which(data$is_train),
		test.inds = which(!data$is_train),
		size = nrow(data)
	),
	measures = auc
)

saveRDS(benchmark_results, "benchmark.rds")
