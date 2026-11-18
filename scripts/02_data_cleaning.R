source("000_header.R")

matches_raw <- readRDS(RAW("statsbomb_matches_raw.rds"))
events_raw  <- readRDS(RAW("statsbomb_events_raw.rds"))

matches_clean <- matches_raw %>%
  # Removing final matches (applies only to NWSL final) because they are held at neutral venues without home advantage
  filter(competition_stage.name != "Final") #%>%
  # # Add a variable to indicate matches played without fans (applies only to FA WSL 2020/2021 season)
  # mutate(
  #   no_fans = if_else(competition.competition_name == "FA Women's Super League" & season.season_name == "2020/2021", 1, 0)
  # )

# 1. Sum shot metrics by match and team
team_match_shots <- events_raw %>%
  # sort to shots only and remove penalty shootouts (period 5)
  filter(type.name == "Shot", period != 5) %>%
  group_by(match_id, team.id) %>%
  summarise(
    shots           = n(),
    shots_on_target = sum(shot.outcome.name %in% c("Goal", "Saved", "Saved to Post"), na.rm = TRUE),
    goals           = sum(shot.outcome.name == "Goal", na.rm = TRUE),
    xg              = sum(shot.statsbomb_xg, na.rm = TRUE),
    npxg            = sum(if_else(shot.type.name != "Penalty", shot.statsbomb_xg, 0), na.rm = TRUE),
    penalties_taken = sum(shot.type.name == "Penalty", na.rm = TRUE),
    gax             = goals - xg,
    goals_per_shot  = if_else(shots > 0, goals / shots, NA_real_),
    npxg_per_shot   = if_else(shots > 0, npxg / shots, NA_real_),
    goals_per_sot   = if_else(shots_on_target > 0, goals / shots_on_target, NA_real_),
    .groups = "drop"
  )

# 2. Sum discipline metrics (fouls, yellow cards, red cards) by match and team
# Note: StatsBomb logs cards across two nested fields: foul_committed.card.name and bad_behaviour.card.name
team_match_discipline <- events_raw %>%
  filter(period != 5) %>%
  group_by(match_id, team.id) %>%
  summarise(
    fouls = sum(type.name == "Foul Committed", na.rm = TRUE),
    yellow_cards = sum(
      foul_committed.card.name %in% c("Yellow Card", "Second Yellow") |
        bad_behaviour.card.name %in% c("Yellow Card", "Second Yellow"),
      na.rm = TRUE
    ),
    red_cards = sum(
      foul_committed.card.name %in% c("Red Card", "Second Yellow") |
        bad_behaviour.card.name %in% c("Red Card", "Second Yellow"),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Combine team-match event summaries into a single lookup table
team_match_events <- full_join(
  team_match_shots,
  team_match_discipline,
  by = c("match_id", "team.id")
)

# 3. Reshape into one row per match
fixture_base <- matches_clean %>%
  select(
    match_id,
    season         = season.season_name,
    match_date,
    competition    = competition.competition_name,
    competition_id = competition.competition_id,
    #no_fans,
    home_team_id   = home_team.home_team_id,
    home_team      = home_team.home_team_name,
    away_team_id   = away_team.away_team_id,
    away_team      = away_team.away_team_name,
    home_score,
    away_score,
    stadium_name   = stadium.name,
    referee_name   = referee.name
  ) %>%
  mutate(match_date = as.Date(match_date))

statsbomb_matches <- fixture_base %>%
  # Join Home metrics (shots + discipline)
  left_join(
    team_match_events %>% rename_with(~ paste0("home_", .), -match_id),
    by = c("match_id", "home_team_id" = "home_team.id")
  ) %>%
  # Join Away metrics (shots + discipline)
  left_join(
    team_match_events %>% rename_with(~ paste0("away_", .), -match_id),
    by = c("match_id", "away_team_id" = "away_team.id")
  ) %>%
  # Fill NAs with 0 for fixtures where a team had 0 recorded shots, fouls, or cards
  # Note: Rate metrics (_per_shot, _per_sot) are kept as NA when shots/shots_on_target = 0
  mutate(across(
    c(starts_with("home_"), starts_with("away_")) &
      where(is.numeric) &
      !c(home_score, away_score, home_team_id, away_team_id,
         ends_with("_per_shot"), ends_with("_per_sot")),
    ~ replace_na(.x, 0)
  )) %>%
  # Construct outcome and differential variables
  mutate(
    score_diff   = home_score - away_score,
    xg_diff      = home_xg - away_xg,
    npxg_diff    = home_npxg - away_npxg,
    gax_diff     = home_gax - away_gax,
    foul_diff    = home_fouls - away_fouls,
    yellow_diff  = home_yellow_cards - away_yellow_cards,
    red_diff     = home_red_cards - away_red_cards,
    home_win     = as.integer(home_score > away_score),
    away_win     = as.integer(away_score > home_score),
    draw         = as.integer(home_score == away_score),
    home_points  = case_when(home_win == 1 ~ 3, draw == 1 ~ 1, TRUE ~ 0),
    away_points  = case_when(away_win == 1 ~ 3, draw == 1 ~ 1, TRUE ~ 0),
    total_points = home_points + away_points
  )

# 4. Build team-match stacked dataset (2 rows per match)
home_rows <- statsbomb_matches %>%
  transmute(
    match_id, competition, competition_id, season, match_date, stadium_name, referee_name, #no_fans,
    team_id         = home_team_id,
    team            = home_team,
    opponent_id     = away_team_id,
    opponent        = away_team,
    is_home         = 1,
    goals           = home_score,
    goals_conceded  = away_score,
    goal_diff       = score_diff,
    shots           = home_shots,
    shots_on_target = home_shots_on_target,
    xg              = home_xg,
    npxg            = home_npxg,
    penalties_taken = home_penalties_taken,
    gax             = home_gax,
    goals_per_shot  = home_goals_per_shot,
    npxg_per_shot   = home_npxg_per_shot,
    goals_per_sot   = home_goals_per_sot,
    fouls           = home_fouls,
    yellow_cards    = home_yellow_cards,
    red_cards       = home_red_cards,
    opp_xg          = away_xg,
    opp_npxg        = away_npxg,
    opp_gax         = away_gax,
    opp_fouls       = away_fouls,
    opp_yellow_cards= away_yellow_cards,
    opp_red_cards   = away_red_cards,
    xg_diff         = xg_diff,
    npxg_diff       = npxg_diff,
    gax_diff        = gax_diff,
    foul_diff       = foul_diff,
    yellow_diff     = yellow_diff,
    red_diff        = red_diff
  )

away_rows <- statsbomb_matches %>%
  transmute(
    match_id, competition, competition_id, season, match_date, stadium_name, referee_name, #no_fans,
    team_id         = away_team_id,
    team            = away_team,
    opponent_id     = home_team_id,
    opponent        = home_team,
    is_home         = 0,
    goals           = away_score,
    goals_conceded  = home_score,
    goal_diff       = -score_diff,
    shots           = away_shots,
    shots_on_target = away_shots_on_target,
    xg              = away_xg,
    npxg            = away_npxg,
    penalties_taken = away_penalties_taken,
    gax             = away_gax,
    goals_per_shot  = away_goals_per_shot,
    npxg_per_shot   = away_npxg_per_shot,
    goals_per_sot   = away_goals_per_sot,
    fouls           = away_fouls,
    yellow_cards    = away_yellow_cards,
    red_cards       = away_red_cards,
    opp_xg          = home_xg,
    opp_npxg        = home_npxg,
    opp_gax         = home_gax,
    opp_fouls       = home_fouls,
    opp_yellow_cards= home_yellow_cards,
    opp_red_cards   = home_red_cards,
    xg_diff         = -xg_diff,
    npxg_diff       = -npxg_diff,
    gax_diff        = -gax_diff,
    foul_diff       = -foul_diff,
    yellow_diff     = -yellow_diff,
    red_diff        = -red_diff
  )

statsbomb_stacked <- bind_rows(home_rows, away_rows) %>%
  mutate(
    win         = as.integer(goal_diff > 0),
    draw        = as.integer(goal_diff == 0),
    loss        = as.integer(goal_diff < 0),
    points      = case_when(win == 1 ~ 3, draw == 1 ~ 1, TRUE ~ 0),
    comp_season = paste(competition, season, sep = "_")
  ) %>%
  arrange(match_id, desc(is_home))

# Save both clean datasets
saveRDS(statsbomb_matches, PROCESSED("statsbomb_matches_clean.rds"))
saveRDS(statsbomb_stacked, PROCESSED("statsbomb_stacked_clean.rds"))