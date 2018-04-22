library(ROCR)
library(tidyverse)

benchmark_pred <- "benchmark.rds" %>% 
	readRDS() %>% 
	mlr::getBMRPredictions()

data <- "data.rds" %>% 
	readRDS() %>% 
	filter(!is_train) %>% 
	transmute(
		basho,
		y,
		mb_open = 1 / odds1_open / (1 / odds1_open + 1 / odds2_open),
		mb_close = 1 / odds1_close / (1 / odds1_close + 1 / odds2_close),
		classif.binomial = benchmark_pred[["select(data, -is_train)"]][["classif.binomial"]][["data"]][["prob.yes"]]
	)

textAUC <- function(pred, x, y, col) text(x, y, sprintf("%.3f", unlist(performance(pred, "auc")@y.values)), c(0, 1), col = col, cex = 2)

par(mfrow = c(3, 3))

sapply(
	c(unique(data$basho), "Overall"),
	function(x) {
		tmp_data <- filter(data, x == "Overall" | basho == x)
		
		mb_open <- with(
			tmp_data,
			prediction(
				predictions = mb_open,
				labels = y
			)
		)
		
		mb_close <- with(
			tmp_data,
			prediction(
				predictions = mb_close,
				labels = y
			)
		)
		
		mine <- with(
			tmp_data,
			prediction(
				predictions = classif.binomial,
				labels = y
			)
		)
		
		plot(performance(mb_open, "tpr", "fpr"), col = "#00953a", lwd = 2, main = x)
		plot(performance(mb_close, "tpr", "fpr"), add = TRUE, col = "#ed1c24", lwd = 2)
		plot(performance(mine, "tpr", "fpr"), add = TRUE, col = "#00c1de", lwd = 2)
		
		lines(x = c(0, 1), y = c(0, 1), lty = 2)
		
		textAUC(mb_open, 0, 1, "#00953a")
		textAUC(mb_close, 0, .92, "#ed1c24")
		textAUC(mine, 0, .84, "#00c1de")
	}
)
