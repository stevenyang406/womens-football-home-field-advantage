source("000_header.R")

matches_clean <- readRDS(PROCESSED("statsbomb_matches_clean.rds"))
stacked_clean <- readRDS(PROCESSED("statsbomb_stacked_clean.rds"))

# Pooled OLS
ols_win <- lm_robust(win ~ is_home, data = stacked_clean, cluster = match_id, se_type = "stata")
ols_xg <- lm_robust(xg_diff ~ is_home, data = stacked_clean, cluster = match_id, se_type = "stata")
ols_npxg <- lm_robust(npxg_diff ~ is_home, data = stacked_clean, cluster = match_id, se_type = "stata")
 
ols_model_list <- list(
  "Win" = ols_win,
  "xG Diff" = ols_xg,
  "npxG Diff" = ols_npxg
)

ols_results_df <- modelsummary(
  ols_model_list,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  gof_omit = ".*",
  output = "data.frame"
)

# Home and Away FE
hafe_win <- lm_robust(
  win ~ is_home,
  fixed_effects = ~ team + opponent,
  data = stacked_clean,
  cluster = match_id,
  se_type = "stata"
)

hafe_xg <- lm_robust(
  xg_diff ~ is_home,
  fixed_effects = ~ team + opponent,
  data = stacked_clean,
  cluster = match_id,
  se_type = "stata"
)

hafe_npxg <- lm_robust(
  npxg_diff ~ is_home,
  fixed_effects = ~ team + opponent,
  data = stacked_clean,
  cluster = match_id,
  se_type = "stata"
)

hafe_model_list <- list(
  "Win" = hafe_win,
  "xG Diff" = hafe_xg,
  "npxG Diff" = hafe_npxg
)

hafe_results_df <- modelsummary(
  hafe_model_list,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  gof_omit = ".*",
  output = "data.frame"
)

panels <- list(
  "Panel A: Pooled OLS" = list(
    "Win"       = ols_win,
    "xG Diff"   = ols_xg,
    "npxG Diff" = ols_npxg
  ),
  "Panel B: Team + Opponent Fixed Effects" = list(
    "Win"       = hafe_win,
    "xG Diff"   = hafe_xg,
    "npxG Diff" = hafe_npxg
  )
)

modelsummary(
  panels,
  shape = "rbind",
  coef_map = c("is_home" = "Home"),
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map = c("nobs", "adj.r.squared"),
  title = "Home Advantage Across Match Outcomes",
  notes = "Standard errors clustered at the match level (Stata CR1 adjustment) in parentheses."
)