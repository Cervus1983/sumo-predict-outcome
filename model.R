library(caret)
library(tidyverse)


# data
banzuke <- read_csv("banzuke.csv")
results <- read_csv("results.csv")
odds <- read_csv("odds.csv")


# functions
source("common.R")
source("features.R")


# complete data set
all_data <- results %>% 
	add_banzuke_info(
		# replace NA height/weight with mean values
		banzuke %>% 
			group_by(id) %>% 
			mutate(
				height = ifelse(is.na(height), mean(height, na.rm = TRUE), height),
				weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)
			)
	) %>% 
	add_age() %>% 
	parse_rank() %>% 
	add_form() %>% 
	add_head_to_head() %>% 
	left_join(., odds)

saveRDS(all_data, "all_data.rds")


# model
trControl <- trainControl(
	method = "cv",
	number = 3,
	returnResamp = "none",
	summaryFunction = twoClassSummary,
	classProbs = TRUE
)

glm_fit <- train(
	rikishi1_win ~ .,
	data = all_data %>% 
		filter(is.na(odds1)) %>% 
		historical() %>% 
		makuuchi() %>% 
		drop_extra_cols() %>% 
		mutate(rikishi1_win = recode(rikishi1_win + 1, "no", "yes")),
	method = "glm",
	trControl = trControl,
	metric = "ROC"
)

save(glm_fit, file = "glm_fit.Rdata")
