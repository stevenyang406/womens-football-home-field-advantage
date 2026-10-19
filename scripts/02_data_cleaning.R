matches_raw <- readRDS(RAW("statsbomb_matches_raw.rds"))
events_raw <- readRDS(RAW("statsbomb_events_raw.rds"))

# Filter to shots and clean
shots_clean <- events_raw %>%
  filter(type.name == "Shot") %>%
  allclean()

matches_clean <- matches_raw %>%
  # Removing final matches (applies only to NWSL final) because it is held at a neutral venue
  filter(competition_stage.name != "Final") %>%
  # Add a variable to indicate matches played without fans (applies only to FA WSL 2020/2021 season)
  mutate(
    no_fans = if_else(competition.competition_name == "FA Women's Super League" & season.season_name == "2020/2021", 1, 0)
  )

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
fixture_base <- matches_clean %>%
  select(
    match_id,
    season = season.season_name,
    match_date,
    competition = competition.competition_name,
    competition_id = competition.competition_id,
    no_fans,
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

home_rows <- statsbomb_matches %>%
  transmute(
    match_id, competition, competition_id, season, match_date, stadium_name, referee_name, no_fans,
    team_id = home_team_id,
    team = home_team,
    opponent_id = away_team_id,
    opponent = away_team,
    is_home = 1,
    goals = home_score,
    goals_conceded = away_score,
    goal_diff = score_diff,
    shots = home_shots,
    shots_on_target = home_shots_on_target,
    xg = home_xg,
    npxg = home_npxg,
    penalties_taken = home_penalties_taken,
    opp_xg = away_xg,
    opp_npxg = away_npxg,
    xg_diff = xg_diff,
    npxg_diff = npxg_diff
  )

away_rows <- statsbomb_matches %>%
  transmute(
    match_id, competition, competition_id, season, match_date, stadium_name, referee_name, no_fans,
    team_id = away_team_id,
    team = away_team,
    opponent_id = home_team_id,
    opponent = home_team,
    is_home = 0,
    goals = away_score,
    goals_conceded = home_score,
    goal_diff = -score_diff,
    shots = away_shots,
    shots_on_target = away_shots_on_target,
    xg = away_xg,
    npxg = away_npxg,
    penalties_taken = away_penalties_taken,
    opp_xg = home_xg,
    opp_npxg = home_npxg,
    xg_diff = -xg_diff,
    npxg_diff = -npxg_diff
  )

statsbomb_stacked <- bind_rows(home_rows, away_rows) %>%
  arrange(match_id, desc(is_home))

# Save both clean datasets
saveRDS(statsbomb_matches, PROCESSED("statsbomb_matches_clean.rds"))
saveRDS(statsbomb_stacked, PROCESSED("statsbomb_stacked_clean.rds"))

