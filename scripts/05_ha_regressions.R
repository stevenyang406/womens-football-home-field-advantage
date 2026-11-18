source("000_header.R")

matches_clean <- readRDS(PROCESSED("statsbomb_matches_clean.rds"))
stacked_clean <- readRDS(PROCESSED("statsbomb_stacked_clean.rds"))

# ==============================================================================
# 1. Core Match Outcomes (Stacked, Team-Season & Opponent-Season FE)
# ==============================================================================
ols_outcomes <- list(
  "Win"       = lm_robust(win ~ is_home,       data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Points"    = lm_robust(points ~ is_home,    data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Goal Diff" = lm_robust(goal_diff ~ is_home, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "xG Diff"   = lm_robust(xg_diff ~ is_home,   data = stacked_clean, cluster = match_id, se_type = "stata"),
  "npxG Diff" = lm_robust(npxg_diff ~ is_home, data = stacked_clean, cluster = match_id, se_type = "stata")
)

fe_outcomes <- list(
  "Win"       = lm_robust(win ~ is_home,       fixed_effects = ~ team:comp_season + opponent:comp_season, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Points"    = lm_robust(points ~ is_home,    fixed_effects = ~ team:comp_season + opponent:comp_season, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Goal Diff" = lm_robust(goal_diff ~ is_home, fixed_effects = ~ team:comp_season + opponent:comp_season, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "xG Diff"   = lm_robust(xg_diff ~ is_home,   fixed_effects = ~ team:comp_season + opponent:comp_season, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "npxG Diff" = lm_robust(npxg_diff ~ is_home, fixed_effects = ~ team:comp_season + opponent:comp_season, data = stacked_clean, cluster = match_id, se_type = "stata")
)

modelsummary(
  list(
    "Panel A: Pooled OLS" = ols_outcomes,
    "Panel B: Team-Season & Opponent-Season Fixed Effects" = fe_outcomes
  ),
  shape = "rbind",
  coef_map = c("is_home" = "Home"),
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map = c("nobs", "adj.r.squared"),
  title = "Home Advantage Across Match Outcomes",
  notes = "Standard errors clustered at the match level (Stata CR1 adjustment) in parentheses.",
  output = OUTPUT("stacked_ha_outcomes.png")
)

# ==============================================================================
# 2. Closed Doors (Match-Level, Home & Away Team FE)
# ==============================================================================
ols_closed <- list(
  "Home Win"  = lm_robust(home_win ~ closed_doors,    data = matches_clean, se_type = "stata"),
  "Home Pts"  = lm_robust(home_points ~ closed_doors, data = matches_clean, se_type = "stata"),
  "Goal Diff" = lm_robust(score_diff ~ closed_doors,  data = matches_clean, se_type = "stata"),
  "xG Diff"   = lm_robust(xg_diff ~ closed_doors,     data = matches_clean, se_type = "stata"),
  "npxG Diff" = lm_robust(npxg_diff ~ closed_doors,   data = matches_clean, se_type = "stata")
)

fe_closed <- list(
  "Home Win"  = lm_robust(home_win ~ closed_doors,    fixed_effects = ~ home_team + away_team, data = matches_clean, se_type = "stata"),
  "Home Pts"  = lm_robust(home_points ~ closed_doors, fixed_effects = ~ home_team + away_team, data = matches_clean, se_type = "stata"),
  "Goal Diff" = lm_robust(score_diff ~ closed_doors,  fixed_effects = ~ home_team + away_team, data = matches_clean, se_type = "stata"),
  "xG Diff"   = lm_robust(xg_diff ~ closed_doors,     fixed_effects = ~ home_team + away_team, data = matches_clean, se_type = "stata"),
  "npxG Diff" = lm_robust(npxg_diff ~ closed_doors,   fixed_effects = ~ home_team + away_team, data = matches_clean, se_type = "stata")
)

modelsummary(
  list(
    "Panel A: Pooled OLS" = ols_closed,
    "Panel B: Home + Away Fixed Effects" = fe_closed
  ),
  shape = "rbind",
  coef_map = c(
    "(Intercept)"  = "Home Average with Fans (Intercept)",
    "closed_doors" = "Closed Doors"
  ),
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map = c("nobs", "adj.r.squared"),
  title = "Effect of Closed Doors on Home Advantage (Match-Level)",
  notes = "Heteroskedasticity-robust (HC1 / Stata) standard errors in parentheses.",
  output = OUTPUT("match_ha_closeddoors.png")
)

# ==============================================================================
# 3. Log Attendance (Match-Level, FA WSL 2018/19–2020/21, N = 324)
# ==============================================================================
matches_att <- dplyr::filter(matches_clean, !is.na(log_attendance))

ols_att <- list(
  "Home Win"  = lm_robust(home_win ~ log_attendance,    data = matches_att, se_type = "stata"),
  "Home Pts"  = lm_robust(home_points ~ log_attendance, data = matches_att, se_type = "stata"),
  "Goal Diff" = lm_robust(score_diff ~ log_attendance,  data = matches_att, se_type = "stata"),
  "xG Diff"   = lm_robust(xg_diff ~ log_attendance,     data = matches_att, se_type = "stata"),
  "npxG Diff" = lm_robust(npxg_diff ~ log_attendance,   data = matches_att, se_type = "stata")
)

fe_att <- list(
  "Home Win"  = lm_robust(home_win ~ log_attendance,    fixed_effects = ~ home_team + away_team, data = matches_att, se_type = "stata"),
  "Home Pts"  = lm_robust(home_points ~ log_attendance, fixed_effects = ~ home_team + away_team, data = matches_att, se_type = "stata"),
  "Goal Diff" = lm_robust(score_diff ~ log_attendance,  fixed_effects = ~ home_team + away_team, data = matches_att, se_type = "stata"),
  "xG Diff"   = lm_robust(xg_diff ~ log_attendance,     fixed_effects = ~ home_team + away_team, data = matches_att, se_type = "stata"),
  "npxG Diff" = lm_robust(npxg_diff ~ log_attendance,   fixed_effects = ~ home_team + away_team, data = matches_att, se_type = "stata")
)

modelsummary(
  list(
    "Panel A: Pooled OLS" = ols_att,
    "Panel B: Home + Away Fixed Effects" = fe_att
  ),
  shape = "rbind",
  coef_map = c(
    "(Intercept)"    = "Home (Closed Doors Baseline)",
    "log_attendance" = "Log Attendance"
  ),
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map = c("nobs", "adj.r.squared"),
  title = "Effect of Stadium Attendance on Home Advantage (Match-Level, FA WSL)",
  notes = "Heteroskedasticity-robust (HC1 / Stata) standard errors in parentheses.",
  output = OUTPUT("match_ha_attendance.png")
)

# ==============================================================================
# 4. Shots & Discipline (Stacked, Team-Season, Opponent-Season & Referee FE)
# ==============================================================================
ols_disc <- list(
  "Shots"           = lm_robust(shots ~ is_home,           data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Shots on Target" = lm_robust(shots_on_target ~ is_home, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Goals"           = lm_robust(goals ~ is_home,           data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Fouls"           = lm_robust(fouls ~ is_home,           data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Yellow Cards"    = lm_robust(yellow_cards ~ is_home,    data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Red Cards"       = lm_robust(red_cards ~ is_home,       data = stacked_clean, cluster = match_id, se_type = "stata")
)

fe_disc <- list(
  "Shots"           = lm_robust(shots ~ is_home,           fixed_effects = ~ team:comp_season + opponent:comp_season + referee_name, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Shots on Target" = lm_robust(shots_on_target ~ is_home, fixed_effects = ~ team:comp_season + opponent:comp_season + referee_name, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Goals"           = lm_robust(goals ~ is_home,           fixed_effects = ~ team:comp_season + opponent:comp_season + referee_name, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Fouls"           = lm_robust(fouls ~ is_home,           fixed_effects = ~ team:comp_season + opponent:comp_season + referee_name, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Yellow Cards"    = lm_robust(yellow_cards ~ is_home,    fixed_effects = ~ team:comp_season + opponent:comp_season + referee_name, data = stacked_clean, cluster = match_id, se_type = "stata"),
  "Red Cards"       = lm_robust(red_cards ~ is_home,       fixed_effects = ~ team:comp_season + opponent:comp_season + referee_name, data = stacked_clean, cluster = match_id, se_type = "stata")
)

modelsummary(
  list(
    "Panel A: Pooled OLS" = ols_disc,
    "Panel B: Team-Season, Opponent-Season & Referee Fixed Effects" = fe_disc
  ),
  shape = "rbind",
  coef_map = c("is_home" = "Home"),
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map = c("nobs", "adj.r.squared"),
  title = "Home Advantage in Shots and Disciplinary Actions",
  notes = "Standard errors clustered at the match level (Stata CR1 adjustment) in parentheses.",
  output = OUTPUT("stacked_ha_shots_discipline.png")
)