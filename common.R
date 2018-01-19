all_ranks <- c("Y", "O", "S", "K", "M", "J", "Ms", "Sd", "Jd", "Jk")


# switches "1" <-> "2" columns of {data}
# if {by} is provided, only switches columns when {by1} > {by2}
switch_columns <- function(data, by = NA) {
	df <- apply(data, 1, function(x) {
		tmp <- names(x)
		
		if (is.na(by) || x[[paste0(by, "1")]] > x[[paste0(by, "2")]]) {
			tmp[grep("1", names(x))] <- names(x)[grep("2", names(x))]
			tmp[grep("2", names(x))] <- names(x)[grep("1", names(x))]
		}
		
		set_names(x[tmp], NULL)
	}) %>% 
		t() %>% 
		as.tibble() %>% 
		set_names(names(data))
	
	df[] <- mapply(
		FUN = as,
		df,
		sapply(data, class),
		SIMPLIFY = FALSE
	)
	
	df
}


# removes records unused in training & evaluation
historical <- function(data) data %>% 
	filter(
		basho > 1989, # remove first 6 years
		kimarite != "fusen", # remove walkovers
		!is.na(rikishi1_win) # remove upcoming bouts
	)
