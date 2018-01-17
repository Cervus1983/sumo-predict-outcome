add_banzuke_info <- function(data) {
	banzuke_relevant_cols <- c("basho", "id", "birth_date", "height", "weight", "prev", "prev_w", "prev_l")

	data %>% 
		inner_join(
			.,
			banzuke %>% 
				select(one_of(banzuke_relevant_cols)) %>% 
				set_names(c("basho", paste("rikishi1", tail(names(.), -1), sep = "_")))
		) %>% 
		inner_join(
			.,
			banzuke %>% 
				select(one_of(banzuke_relevant_cols)) %>% 
				set_names(c("basho", paste("rikishi2", tail(names(.), -1), sep = "_")))
		)
}
