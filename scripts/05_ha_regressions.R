source("000_header.R")

matches_clean <- readRDS(PROCESSED("statsbomb_matches_clean.rds"))
stacked_clean <- readRDS(PROCESSED("statsbomb_stacked_clean.rds"))

run_match_fe_panels <- function(
    outcomes,
    rhs,
    coef_map,
    data = matches_clean,
    title = "Home Advantage: Match-Level Pooled OLS vs. Home & Away Team Fixed Effects",
    notes = "Heteroskedasticity-robust (HC1 / Stata) standard errors in parentheses.",
    output = "default"
) {
  # Panel A: Pooled OLS
  ols_models <- purrr::map(outcomes, function(y) {
    fml <- stats::as.formula(paste(y, "~", rhs))
    estimatr::lm_robust(fml, data = data, se_type = "stata")
  })
  
  # Panel B: Home Team + Away Team Fixed Effects (Szabó & Kerényi, 2023 specification)
  hafe_models <- purrr::map(outcomes, function(y) {
    fml <- stats::as.formula(paste(y, "~", rhs))
    estimatr::lm_robust(
      fml,
      fixed_effects = ~ home_team + away_team,
      data = data,
      se_type = "stata"
    )
  })
  
  panels <- list(
    "Panel A: Pooled OLS" = ols_models,
    "Panel B: Home + Away Team Fixed Effects" = hafe_models
  )
  
  modelsummary(
    panels,
    shape = "rbind",
    coef_map = coef_map,
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    gof_map = c("nobs", "adj.r.squared"),
    title = title,
    notes = notes,
    output = output
  )
}

run_fe_panels <- function(
    outcomes,
    rhs = "is_home",
    coef_map = c("is_home" = "Home"),
    data = stacked_clean,
    title = "Home Advantage: Pooled OLS vs. Team & Opponent Fixed Effects",
    notes = "Standard errors clustered at the match level (Stata CR1 adjustment) in parentheses.",
    output = "default"
) {
  # Fit Pooled OLS for each outcome
  ols_models <- purrr::map(outcomes, function(y) {
    fml <- stats::as.formula(paste(y, "~", rhs))
    estimatr::lm_robust(fml, data = data, cluster = match_id, se_type = "stata")
  })
  
  # Fit Team + Opponent Fixed Effects (by season) for each outcome
  hafe_models <- purrr::map(outcomes, function(y) {
    fml <- stats::as.formula(paste(y, "~", rhs))
    estimatr::lm_robust(
      fml,
      fixed_effects = ~ team:comp_season + opponent:comp_season,
      data = data,
      cluster = match_id,
      se_type = "stata"
    )
  })
  
  panels <- list(
    "Panel A: Pooled OLS" = ols_models,
    "Panel B: Team + Opponent Fixed Effects by Season" = hafe_models
  )
  
  modelsummary(
    panels,
    shape = "rbind",
    coef_map = coef_map,
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    gof_map = c("nobs", "adj.r.squared"),
    title = title,
    notes = notes,
    output = output
  )
}

# Match outcomes
run_fe_panels(
  outcomes = c("Win" = "win",
               "Points" = "points",
               "Goal Diff" = "goal_diff",
               "xG Diff" = "xg_diff",
               "npxG Diff" = "npxg_diff"
               ),
  title = "Home Advantage Across Match Outcomes",
  output = OUTPUT("stacked_ha_outcomes.png")
)

# Match outcomes with attendance

run_match_fe_panels(
  outcomes = c(
    "Home Win"  = "home_win",
    "Home Pts"  = "home_points",
    "Goal Diff" = "score_diff",
    "xG Diff"   = "xg_diff",
    "npxG Diff" = "npxg_diff"
  ),
  rhs = "closed_doors",
  coef_map = c(
    "(Intercept)"  = "Home Average with Fans (Intercept)",
    "closed_doors" = "Closed Doors"
  ),
  title = "Effect of Closed Doors on Home Advantage (Match-Level)",
  output = OUTPUT("match_ha_closeddoors.png")
)

# 2. Log Attendance Regression on Unstacked Matches (FA WSL 2018/19–2020/21, N = 324)
run_match_fe_panels(
  outcomes = c(
    "Home Win"  = "home_win",
    "Home Pts"  = "home_points",
    "Goal Diff" = "score_diff",
    "xG Diff"   = "xg_diff",
    "npxG Diff" = "npxg_diff"
  ),
  rhs = "log_attendance",
  coef_map = c(
    "(Intercept)"    = "Home (Closed Doors Baseline)",
    "log_attendance" = "Log Attendance"
  ),
  data = dplyr::filter(matches_clean, !is.na(log_attendance)),
  title = "Effect of Stadium Attendance on Home Advantage (Match-Level, FA WSL)",
  output = OUTPUT("match_ha_attendance.png")
)

# Shots & discipline
run_fe_panels(
  outcomes = c("Shots" = "shots",
               "Shots on Target" = "shots_on_target",
               "Goals" = "goals",
               "Fouls" = "fouls",
               "Yellow Cards" = "yellow_cards",
               "Red Cards" = "red_cards"
               ),
  title = "Home Advantage in Shots and Disciplinary Actions",
  output = OUTPUT("stacked_ha_shots_discipline.png")
)