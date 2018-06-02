library(mlr)
library(ROCR)
library(tidyverse)

data <- "data.rds" %>% 
	readRDS() %>% 
	filter(!is.na(odds1_open)) %>% 
	mutate(y = recode(rikishi1_win + 1, "no", "yes")) %>% 
	mutate_if(is.character, as.factor) %>% 
	mutate_if(is.ordered, as.integer)

task <- makeClassifTask(
	id = "my_task",
	data = data %>% 
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
		),
	target = "y",
	positive = "yes"
)

data <- data %>% 
	mutate(
		open = 1 / odds1_open / (1 / odds1_open + 1 / odds2_open),
		close = 1 / odds1_close / (1 / odds1_close + 1 / odds2_close),
		model = "model.rds" %>% 
			readRDS() %>% 
			predict(task) %>% 
			.$data %>% 
			pull(prob.yes)
	)
	
	
textAUC <- function(pred, x, y, col) text(x, y, sprintf("%.3f", unlist(performance(pred, "auc")@y.values)), c(0, 1), col = col, cex = 2)

par(mfrow = rep(ceiling(sqrt(length(unique(data$basho)) + 1)), 2))

sapply(
	c(unique(data$basho), "Overall"),
	function(x) {
		tmp_data <- filter(data, x == "Overall" | basho == x)
		
		open <- with(
			tmp_data,
			prediction(
				predictions = open,
				labels = y
			)
		)
		
		close <- with(
			tmp_data,
			prediction(
				predictions = close,
				labels = y
			)
		)
		
		model <- with(
			tmp_data,
			prediction(
				predictions = model,
				labels = y
			)
		)
		
		plot(performance(open, "tpr", "fpr"), col = "#00953a", lwd = 2, main = x)
		plot(performance(close, "tpr", "fpr"), add = TRUE, col = "#ed1c24", lwd = 2)
		plot(performance(model, "tpr", "fpr"), add = TRUE, col = "#00c1de", lwd = 2)
		
		lines(x = c(0, 1), y = c(0, 1), lty = 2)
		
		textAUC(open, 0, 1, "#00953a")
		textAUC(close, 0, .92, "#ed1c24")
		textAUC(model, 0, .84, "#00c1de")
	}
)
