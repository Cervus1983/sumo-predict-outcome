test_data %>% 
	mutate(p = 1 / odds1_close / (1 / odds1_close + 1 / odds2_close)) %>% 
	ggplot(aes(p, colour = as.factor(rikishi1_win))) +
		geom_density() +
		facet_wrap(~basho)
