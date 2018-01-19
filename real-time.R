all_data %>% 
	filter(
		is.na(rikishi1_win),
		!is.na(odds1)
	) %>% 
	head(2) %>% 
	tail(1) %>% 
	drop_extra_cols() %>% 
	unlist() %>% 
	paste(collapse = ", ")
