always_underdog <- function(...) {
	odds1 <- as.numeric(list(...)[[1]][["odds1"]])
	odds2 <- as.numeric(list(...)[[1]][["odds2"]])
	
	switch(
		sign(odds1 - odds2) + 2,
		2, 0, 1
	)
}

simulate <- function(data, strategy) mutate(
	data,
	gross = recode(
		apply(data, 1, strategy) + 1,
		0, odds1 * rikishi1_win - 1, odds2 * rikishi2_win - 1
	)
)

inner_join(odds, results) %>% 
	simulate(always_underdog) %>% 
	group_by(basho) %>% 
	summarise(n(), sum(gross))
