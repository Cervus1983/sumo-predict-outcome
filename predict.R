source("common.R")
source("data.world.R")
source("features.R")


# banzuke ----
banzuke <- read_csv("banzuke.csv")


# current odds ----
library(httr)

source("../sumo-odds/betmarathon-parse.R")

current_odds <- "https://www.betmarathon.com/en/betting/Sumo/?menu=954952" %>% 
	GET() %>% 
	content(., "text") %>% 
	extract_odds() %>% 
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


# upcoming bouts ----
results <- dataset_as_data_frame("sumo-results")

upcoming <- results %>% 
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
	add_head_to_head() %>% 
	filter(is.na(rikishi1_win)) %>% 
	inner_join(., rbind(current_odds, switch_columns(current_odds)))


# predictions & expected value ----
load("glm_fit.Rdata")

upcoming %>% 
	mutate(
		rikishi1_win_prob = predict(glm_fit, upcoming, "prob")$yes,
		rikishi1_bet_ev = rikishi1_win_prob * odds1 - 1
	) %>% 
	View()
