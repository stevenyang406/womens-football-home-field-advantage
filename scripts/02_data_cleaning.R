matches_raw <- readRDS(RAW("statsbomb_wsl_matches_raw.rds"))
events_raw <- readRDS(RAW("statsbomb_wsl_events_raw.rds"))

# Filter to shots and clean
shots_clean <- events_raw %>%
  filter(type.name == "Shot") %>%
  allclean()

# Sum shot metrics by match and team
team_match_shots <- shots_clean  %>%
  group_by(match_id, team.id) %>%
  summarise(
    shots = n(),
    shots_on_target = sum(shot.outcome.name %in% c("Goal", "Saved", "Saved to Post"), na.rm = TRUE),
    goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
    xg = sum(shot.statsbomb_xg, na.rm = TRUE),
    npxg = sum(if_else(shot.type.name != "Penalty", shot.statsbomb_xg, 0), na.rm = TRUE),
    penalties_taken = sum(shot.type.name == "Penalty", na.rm = TRUE),
    .groups = "drop"
  )

# Reshape into one row per match
fixture_base <- matches_raw %>%
  select(
    match_id,
    season = season.season_name,
    match_date,
    competition = competition.competition_name,
    competition_id = competition.competition_id,
    home_team_id = home_team.home_team_id,
    home_team = home_team.home_team_name,
    away_team_id = away_team.away_team_id,
    away_team = away_team.away_team_name,
    home_score,
    away_score,
    stadium_name = stadium.name,
    referee_name = referee.name
  ) %>%
  mutate(match_date = as.Date(match_date))

statsbomb_matches <- fixture_base %>%
  # Join Home metrics
  left_join(
    team_match_shots %>% rename_with(~ paste0("home_", .), -match_id),
    by = c("match_id", "home_team_id" = "home_team.id")
  ) %>%
  # Join Away metrics
  left_join(
    team_match_shots %>% rename_with(~ paste0("away_", .), -match_id),
    by = c("match_id", "away_team_id" = "away_team.id")
  ) %>%
  # Fill NAs with 0 for fixtures where a team had 0 recorded shots
  mutate(across(
    c(starts_with("home_"), starts_with("away_")) & 
      where(is.numeric) & 
      !c(home_score, away_score, home_team_id, away_team_id),
    ~ replace_na(.x, 0)
  )) %>%
  # Construct outcome variables
  mutate(
    score_diff = home_score - away_score,
    xg_diff = home_xg - away_xg,
    npxg_diff = home_npxg - away_npxg
  )

saveRDS(statsbomb_matches, PROCESSED("statsbomb_wsl_matches_clean.rds"))
