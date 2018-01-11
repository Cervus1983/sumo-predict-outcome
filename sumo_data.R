library(data.world)
#saved_cfg <- save_config("eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmNlcnZ1cyIsImlzcyI6ImFnZW50OmNlcnZ1czo6MmEzMjVjMTctZjlkZi00YjIxLWJjY2UtMzE2MjUzYjA1ODdiIiwiaWF0IjoxNTEzNTk2MzE1LCJyb2xlIjpbInVzZXJfYXBpX3JlYWQiLCJ1c2VyX2FwaV93cml0ZSJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.ASX8RA8BH33s5IsNcJMuvl4UDIxmhVZWj8OdFamEoPxNjglOe_XXJI9CodrYK7m7958NVM9XSh_RTwa7ZQpBcA")
set_config(saved_cfg)

library(lubridate)
library(tidyverse)

source("common.R")

options(stringsAsFactors = FALSE)

# 18 years ~ 107 tournaments (2011 March cancelled)
basho <- expand.grid(2000:2017, sprintf("%02d", seq(1, 12, by = 2))) %>% 
	apply(., 1, paste, collapse = ".") %>% 
	setdiff(., "2011.03") %>% 
	sort()

# loads dataset
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

# banzuke
banzuke <- get_data("cervus/sumo-wrestling-banzuke")

# results with play-offs & walkovers
results <- get_data("cervus/sumo-wrestling-results") %>% 
	# switch columns (lower id | higher id)
	mutate(
		id1 = rikishi1_id,
		id2 = rikishi2_id
	) %>% 
	switch_columns(by = "id") %>% 
	select(-id1, -id2)

# opening odds
odds <- get_data("cervus/sumo-wrestling-betting-odds") %>% 
	mutate(basho = ts %>% substr(., 1, 7) %>% sub("-", ".", .) %>% as.numeric()) %>% 
	group_by(basho, rikishi1, rikishi2) %>% 
	summarise(
		odds1 = first(odds1),
		odds2 = first(odds2)
	) %>% 
	ungroup() %>% 
	# add rikishi id & switch columns (lower id | higher id)
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
	) %>% 
	mutate(
		id1 = rikishi1_id,
		id2 = rikishi2_id
	) %>% 
	switch_columns(by = "id") %>% 
	select(-id1, -id2) %>% 
	# remove play-offs & walkovers, add day #
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

save(
	banzuke, odds, results,
	file = "sumo.RData"
)
