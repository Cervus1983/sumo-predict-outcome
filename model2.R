library(caret)

source("common.R")
source("sumo_features.R")


# enriched dataset (last 6 years)
load("sumo.RData")

df <- results %>% 
	left_join(., odds) %>% 
	add_current_tournament() %>% 
	mutate(
		rikishi1_win_prc = ifelse(
			rikishi1_won + rikishi1_lost > 0,
			rikishi1_won / (rikishi1_won + rikishi1_lost),
			.5
		),
		rikishi2_win_prc = ifelse(
			rikishi2_won + rikishi2_lost > 0,
			rikishi2_won / (rikishi2_won + rikishi2_lost),
			.5
		)
	) %>% 
	add_hth_wins() %>% 
	parse_rank() %>% 
	filter(
		basho > 2012,
		day %in% 1:15,
		kimarite != "fusen",
		(rikishi1_rank_name < "J" | rikishi2_rank_name < "J")
	) %>% 
	# target variable
	mutate(y = recode(rikishi1_win + 1, "no", "yes"))


# features
x <- expand.grid(
	c("rikishi1", "rikishi2"),
	c("rank_name", "rank_level", "win_prc", "fusen", "hth_wins")
) %>% 
	mutate(Var3 = paste(Var1, Var2, sep = "_")) %>% 
	pull(Var3)


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

model2 <- train(
	y ~ .
		+ rikishi1_rank_name:rikishi2_rank_name
		+ rikishi1_rank_name:rikishi1_rank_level
		+ rikishi2_rank_name:rikishi2_rank_level,
	data = df_train,
	method = "glm",
	trControl = trControl,
	metric = "ROC"
)

save(
	df_train, df_test, model2,
	file = "model2.RData"
)
