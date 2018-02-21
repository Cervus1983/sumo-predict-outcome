library(ROCR)


# functions
source("common.R")
textAUC <- function(pred, x, y, col) text(x, y, sprintf("%.3f", unlist(performance(pred, "auc")@y.values)), c(0, 1), col = col, cex = 2)


# data set with predictions (created in benchmark.R)
benchmark_data <- "benchmark_data.rds" %>% 
	readRDS() %>% 
	rename(mine = xgboost)


# ROC plots with AUC values
par(mfrow = c(2, 3))

sapply(
	c(unique(benchmark_data$basho), "Overall"),
	function(x) {
		if (x == "Overall") tmp_data <- benchmark_data else tmp_data <- benchmark_data %>% filter(basho == x)
		
		mb_open <- with(
			tmp_data,
			prediction(
				predictions = mb_open,
				labels = rikishi1_win
			)
		)
		
		mb_close <- with(
			tmp_data,
			prediction(
				predictions = mb_close,
				labels = rikishi1_win
			)
		)
		
		mine <- with(
			tmp_data,
			prediction(
				predictions = mine,
				labels = rikishi1_win
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
