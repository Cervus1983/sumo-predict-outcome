source("features.R")

# csv.R ->
banzuke <- read_csv("csv/banzuke.csv")
results <- read_csv("csv/results.csv")
odds <- read_csv("csv/odds.csv")

# -> data.rds
results %>% 
	# generate features (requires complete data set)
	add_banzuke_info(
		# replace NA height/weight with mean values
		banzuke %>% 
			group_by(id) %>% 
			mutate(
				height = ifelse(is.na(height), mean(height, na.rm = TRUE), height),
				weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)
			)
	) %>% 
	add_age() %>% 
	parse_rank() %>% 
	add_form() %>% 
	add_kachi_koshi() %>% 
	add_streak() %>% 
	add_head_to_head() %>% 
	# keep relevant observations
	filter(
		basho > 1989, # remove first 6 years
		day < 16, # remove play-offs
		kimarite != "fusen", # remove walkovers
		!is.na(rikishi1_win), # remove upcoming bouts
		rikishi1_rank_name < "J" | rikishi2_rank_name < "J" # remove bouts outside makuuchi (top division)
	) %>% 
	# add odds
	left_join(., odds) %>% 
	# save
	saveRDS("data.rds")
