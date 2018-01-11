library(caret)

source("common.R")
source("sumo_features.R")


# enriched dataset (last 6 years)
load("sumo.RData")

df <- results %>% 
	left_join(., odds) %>% 
	add_hth_wins() %>% 
	parse_rank() %>% 
	filter(
		basho > 2012,
		kimarite != "fusen",
		(rikishi1_rank_name < "J" | rikishi2_rank_name < "J")
	) %>% 
	# target variable
	mutate(y = recode(rikishi1_win + 1, "no", "yes"))


# features
x <- c(
	"rikishi1_rank_name", "rikishi1_rank_level", "rikishi1_hth_wins",
	"rikishi2_rank_name", "rikishi2_rank_level", "rikishi2_hth_wins"
)


# train dataset
df_train <- df %>% 
	filter(is.na(odds1)) %>% 
	select(one_of(c(x, "y"))) %>% 
	mutate_if(is.factor, as.character)


# combine with own mirror replica to remove chance(?) assymetry
df_train <-	rbind(
	df_train,
	df_train %>% 
		switch_columns() %>% 
		mutate(y = recode(y, "yes" = "no", "no" = "yes"))
)


# test dataset (keep all columns)
df_test <- df %>% filter(!is.na(odds1))


# model
trControl <- trainControl(
	method = "cv",
	number = 3,
	returnResamp = "none",
	summaryFunction = twoClassSummary,
	classProbs = TRUE
)

model1 <- train(
	df_train %>% select(-y),
	df_train %>% pull(y),
	method = "glm",
	trControl = trControl,
	metric = "ROC"
)

save(
	df_train, df_test, model1,
	file = "model1.RData"
)
