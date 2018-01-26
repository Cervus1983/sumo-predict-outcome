source("common.R")
source("data.world.R")
source("features.R")


# banzuke ----
banzuke <- read_csv("banzuke.csv")


# upcoming bouts ----
results <- dataset_as_data_frame("sumo-results")

# add features & remove settled bouts
unsettled <- results %>% 
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
	filter(is.na(rikishi1_win))

# upcoming day in top division
upcoming <- unsettled %>% 
	filter(day == max(unsettled$day)) %>% 
	makuuchi()


# current odds ----
library(httr)

source("../sumo-odds/betmarathon-parse.R")

current_odds <- "https://www.marathonbet.com/en/betting/Sumo/Japan/?menu=954984" %>% 
	# fetch & parse HTML
	GET() %>% 
	content(., "text") %>% 
	extract_odds() %>% 
	# infer basho (yyyy.mm) from timestamp
	mutate(basho = ts %>% substr(., 1, 7) %>% sub("-", ".", .) %>% as.numeric()) %>% 
	# add rikishi id
	inner_join(., banzuke %>% transmute(basho, rikishi1 = rikishi, rikishi1_id = id)) %>% 
	inner_join(., banzuke %>% transmute(basho, rikishi2 = rikishi, rikishi2_id = id))


# predictions & expected value ----
load("glm_fit.RData")

inner_join(upcoming, current_odds) %>% 
	mutate(
		rikishi1_win_prob = predict(glm_fit, inner_join(upcoming, current_odds), "prob")$yes,
		rikishi1_bet_ev = rikishi1_win_prob * odds1 - 1
	) %>% 
	select(
		rikishi1_bet_ev, rikishi1_win_prob,
		rikishi1_rank, rikishi1_shikona,
		odds1, odds2,
		rikishi2_rank, rikishi2_shikona,
		rikishi1_result, rikishi2_result,
		rikishi1_head_to_head_wins,
		rikishi2_head_to_head_wins
	) %>% 
	View()
