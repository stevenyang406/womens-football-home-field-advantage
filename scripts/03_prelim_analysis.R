matches_clean <- readRDS(PROCESSED("statsbomb_matches_clean.rds"))
stacked_clean <- readRDS(PROCESSED("statsbomb_stacked_clean.rds"))

## Basic OLS

ols <- feols(xg_diff ~ is_home, data = stacked_clean, cluster = ~match_id)