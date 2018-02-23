source("data.world.R")

# banzuke
banzuke <- dataset_as_data_frame("sumo-banzuke")
write_csv(banzuke, "csv/banzuke.csv")

# results
results <- dataset_as_data_frame("sumo-results")
write_csv(results, "csv/results.csv")

# odds
odds <- dataset_as_data_frame("sumo-wrestling-betting-odds") %>% 
	mutate(basho = ts %>% substr(., 1, 7) %>% sub("-", ".", .) %>% as.numeric()) %>% 
	group_by(basho, rikishi1, rikishi2) %>% 
	arrange(ts) %>% 
	summarise(
		odds1_open = first(odds1),
		odds2_open = first(odds2),
		odds1_close = last(odds1),
		odds2_close = last(odds2)
	) %>% 
	ungroup() %>% 
	# add rikishi id
	inner_join(
		.,
		banzuke %>% 
			transmute(
				basho,
				rikishi1 = rikishi,
				rikishi1_id = id
			)
	) %>% 
	inner_join(
		.,
		banzuke %>% 
			transmute(
				basho,
				rikishi2 = rikishi,
				rikishi2_id = id
			)
	)

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

# add mirror replica
odds <- rbind(odds, switch_columns(odds))

# remove play-offs & walkovers, add day #
odds <- odds %>% 
	inner_join(., results %>% filter(day %in% 1:15, kimarite != "fusen")) %>% 
		select(
			basho,
			day,
			rikishi1_id,
			rikishi1_shikona,
			odds1_open,
			odds2_open,
			odds1_close,
			odds2_close,
			rikishi2_id,
			rikishi2_shikona
		)

write_csv(odds, "csv/odds.csv")
