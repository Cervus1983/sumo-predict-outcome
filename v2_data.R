source("../sumodb/sumodb.R")


# banzuke
basho <- expand.grid(2000:2017, sprintf("%02d", seq(1, 12, by = 2))) %>% 
	apply(., 1, paste, collapse = ".") %>% 
	setdiff(., "2011.03") %>% 
	sort()

banzuke <- do.call(rbind, lapply(basho, function(x) sumodbBanzuke(x)))

write_csv(banzuke, "banzuke.csv")


# results
results <- do.call(rbind, lapply(2000:2017, function(x) sumodbBout(x, division = c("m", "j"))))

write_csv(results, "results.csv")


# odds
source("common.R")

library(data.world)
set_config(cfg_env(auth_token_var = "DW_API_TOKEN"))

get_data <- function(dataset) do.call(
	rbind,
	lapply(
		get_dataset(dataset)$files,
		function(x) {
			counter <- 0
			df <- NULL
			
			while (counter < 10 & is.null(df)) {
				counter <- counter + 1
				
				df <- tryCatch(
					download_file_as_data_frame(dataset, x$name),
					error = function(e) {},
					warning = function(w) {}
				)
				
				Sys.sleep(1)
			}
			
			df
		}
	)
)

odds <- get_data("cervus/sumo-wrestling-betting-odds") %>% 
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
odds <- rbind(odds, switch_columns(odds))

# remove play-offs & walkovers, add day #
odds <- odds %>% 
	inner_join(
		.,
		results %>% 
			filter(
				day %in% 1:15,
				kimarite != "fusen"
			)
	) %>% 
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
