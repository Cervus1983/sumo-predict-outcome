library(Matrix)
library(mlr)
library(xgboost)


# functions
source("common.R")


# data (prepared in models/glm.R)
all_data <- readRDS("all_data.rds")


# train data set
x_train <- sparse.model.matrix(
	rikishi1_win ~ . - 1,
	data = all_data %>% 
		filter(is.na(odds1_open)) %>% 
		historical() %>% 
		makuuchi() %>% 
		select(one_of(c(xgboost_predictors, "rikishi1_win")))
)

y_train = all_data %>% 
	filter(is.na(odds1_open)) %>% 
	historical() %>% 
	makuuchi() %>% 
	pull(rikishi1_win)

xy_train <- xgb.DMatrix(
	data = x_train,
	label = y_train
)


# test data set
x_test <- sparse.model.matrix(
	rikishi1_win ~ . - 1,
	data = all_data %>% 
		filter(!is.na(odds1_open)) %>% 
		select(one_of(c(xgboost_predictors, "rikishi1_win")))
)

y_test = all_data %>% 
	filter(!is.na(odds1_open)) %>% 
	pull(rikishi1_win)

xy_test <- xgb.DMatrix(
	data = x_test,
	label = y_test
)




df <- all_data %>% 
	historical() %>% 
	makuuchi() %>% 
	select(one_of(c(xgboost_predictors, "rikishi1_win", "odds1_open"))) %>% 
	mutate_if(is.character, as.factor)

df <- df %>% 
	createDummyFeatures(
		target = "rikishi1_win",
		cols = df %>% 
			select_if(is.factor) %>% 
			names()
	)

df_train <- df %>% 
	filter(is.na(odds1_open)) %>% 
	select(-odds1_open)

df_test <- df %>% 
	filter(!is.na(odds1_open)) %>% 
	select(-odds1_open)

trainTask <- makeClassifTask(
	data = df_train,
	target = "rikishi1_win",
	positive = 1
)

testTask <- makeClassifTask(
	data = df_test,
	target = "rikishi1_win"
)

xgb_learner <- makeLearner(
	"classif.xgboost",
	predict.type = "prob",
	par.vals = list(
		objective = "binary:logistic",
		eval_metric = "auc",
		nrounds = 200
	)
)

xgb_model <- train(xgb_learner, task = trainTask)

result <- predict(xgb_model, testTask)

xgb_params <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 500),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .1, upper = .5),
  # L2 regularization - prevents overfitting
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)

control <- makeTuneControlRandom(maxit = 5)

resample_desc <- makeResampleDesc("CV", iters = 5)

tuned_params <- tuneParams(
	learner = xgb_learner,
	task = trainTask,
	resampling = resample_desc,
	par.set = xgb_params,
	control = control
)



params <- list(
	colsample_bytree = 1,
	eta = .05,
	gamma = .3,
	max_depth = 4,
	min_child_weight = 10,
	objective = "binary:logistic",
	subsample = .5
)

xgbcv <- xgb.cv(
	params,
	xy_train,
	nrounds = 1e+3,
	nfold = 5,
	metrics = list("auc"),
	stratified = TRUE,
	early_stopping_rounds = 10
)

[1000]	train-auc:0.684619+0.000775	test-auc:0.661628+0.003364
[476]	train-auc:0.684196+0.000688	test-auc:0.662137+0.003030
[229]	train-auc:0.683019+0.000682	test-auc:0.660665+0.002210
[291]	train-auc:0.692793+0.000452	test-auc:0.662322+0.002614

# eXtreme Gradient Boosting
xgboost_fit <- xgb.train(
	params,
	xy_train,
	nrounds = 290,
	watchlist = list(train = xy_train),
	early_stopping_rounds = 10,
	eval_metric = "auc"
)

xgb.save(xgboost_fit, "models/xgboost.model")







confusionMatrix(
	ifelse(predict(xgboost_fit, xy_test) > .5, 1, 0),
	y_test
)




mat <- xgb.importance(
	feature_names = colnames(x_train),
	model = xgb1
)

xgb.plot.importance(mat) 




system.time(
	xgboost_fit <- xgboost(
		xy_train,
		params = params,
		nrounds = 20,
		eval_metric = "auc",
		save_name = "models/xgboost.model"
	)
)
		
		xgboost(
		data = x_train,
		label = y_train,
		#booster = "gblinear",
		#eta = .1,
		nrounds = 10,
		#nthread = 2,
		objective = "binary:logistic",
		save_name = "models/xgboost.model"
	)
)


# tree
xgb.plot.tree(model = xgboost_fit)


# variable importance
xgb.plot.importance(
	xgb.importance(
		feature_names = colnames(x_train),
		model = xgboost_fit
	)
)


# AUC
xgboost_fit %>% 
	predict(x_test) %>% 
	roc(y_test, .) %>% 
	auc()
