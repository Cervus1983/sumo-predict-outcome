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
	#add_rank_vs_rank() %>% 
	add_form() %>% 
	add_kachi_koshi() %>% 
	#add_wins_before() %>% 
	#add_win_rate_before() %>% 
	#add_win_rate_needed() %>% 
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
	# extra columns for ML
	mutate(
		is_train = is.na(odds1_open),
		y = recode(rikishi1_win + 1, "no", "yes")
	) %>% 
	# character variables -> factors
	mutate_if(is.character, as.factor) %>% 
	# save
	saveRDS("data.rds")
