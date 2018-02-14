library(Matrix)
library(tidyverse)
library(xgboost)


# data (prepared in model.R)
all_data <- readRDS("all_data.rds")


# functions
source("common.R")


# train & test data sets
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

x_test <- sparse.model.matrix(
	rikishi1_win ~ . - 1,
	data = all_data %>% 
		filter(!is.na(odds1_open)) %>% 
		select(one_of(c(xgboost_predictors, "rikishi1_win")))
)

y_test = all_data %>% 
	filter(!is.na(odds1_open)) %>% 
	pull(rikishi1_win)


# eXtreme Gradient Boosting
system.time(
	xgboost_fit <- xgboost(
		data = x_train,
		label = y_train,
		eta = .1,
		nrounds = 30,
		#nthread = 2,
		objective = "binary:logistic"
	)
)


# variable importance
xgb.plot.importance(
	xgb.importance(
		feature_names = colnames(x_train),
		model = xgboost_fit
	)
)
