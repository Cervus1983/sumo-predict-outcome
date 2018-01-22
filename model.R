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
		# replace NA height/weight with average values
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

write_csv(all_data, "all_data.csv")


# reading from CSV file causes train(...) to fail
# all_data <- read_csv("all_data.csv", guess_max = 1e+4)


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
		historical() %>% 
		makuuchi() %>% 
		drop_extra_cols() %>% 
		mutate(rikishi1_win = recode(rikishi1_win + 1, "no", "yes")),
	method = "glm",
	trControl = trControl,
	metric = "ROC"
)

saveRDS(glm_fit, file = "glm_fit.rds")


# train & test data sets
all_data %>% 
	historical() %>% 
	filter(is.na(odds1)) %>% 
	drop_extra_cols() %>% 
	write_csv(., "train.csv")

all_data %>% 
	historical() %>% 
	filter(!is.na(odds1)) %>% 
	drop_extra_cols() %>% 
	write_csv(., "test.csv")

all_data %>% 
	historical() %>% 
	filter(!is.na(odds1)) %>% 
	write_csv(., "test2.csv")
