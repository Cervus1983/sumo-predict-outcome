source("data.world.R")

# banzuke
banzuke <- dataset_as_data_frame("sumo-banzuke")
write_csv(banzuke, "banzuke.csv")

# results
results <- dataset_as_data_frame("sumo-results")
write_csv(results, "results.csv")

# odds
odds <- dataset_as_data_frame("sumo-wrestling-betting-odds") %>% 
	mutate(basho = ts %>% substr(., 1, 7) %>% sub("-", ".", .) %>% as.numeric()) %>% 
	group_by(basho, rikishi1, rikishi2) %>% 
	summarise(
		odds1 = first(odds1),
		odds2 = first(odds2)
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

# add mirror replica
source("common.R")
odds <- rbind(odds, switch_columns(odds))

# remove play-offs & walkovers, add day #
odds <- odds %>% 
	inner_join(., results %>% filter(day %in% 1:15, kimarite != "fusen")) %>% 
		select(
			basho,
			day,
			rikishi1_id,
			rikishi1_shikona,
			odds1,
			odds2,
			rikishi2_id,
			rikishi2_shikona
		)

write_csv(odds, "odds.csv")