source("000_header.R")

szabo_raw     <- openxlsx::read.xlsx(RAW("szabo_raw.xlsx"), sheet = 1)
matches_clean <- readRDS(PROCESSED("statsbomb_matches_clean.rds"))
stacked_clean <- readRDS(PROCESSED("statsbomb_stacked_clean.rds"))

# 1. Helper to standardize team names between StatsBomb and Szabo/FBref
clean_team_name <- function(x) {
  x %>%
    str_remove_all("\\s*(WFC|FCW|Women|Ladies|LFC|FC|W)\\b") %>%
    str_replace_all("Brighton & Hove Albion", "Brighton") %>%
    str_replace_all("Tottenham Hotspur", "Tottenham") %>%
    str_replace_all("West Ham United", "West Ham") %>%
    str_replace_all("Manchester United", "Manchester Utd") %>%
    str_replace_all("Birmingham City", "Birmingham") %>%
    str_replace_all("Bristol City", "Bristol") %>%
    str_replace_all("Leicester City", "Leicester") %>%
    str_squish()
}

# 2. Prepare Szabo England (FA WSL) attendance lookup (459 matches, 2018/19 - 2021/22)
szabo_wsl <- szabo_raw %>%
  filter(League == "England") %>%
  mutate(
    season_std    = str_replace_all(Season, "–|-", "/"),
    home_team_std = clean_team_name(Home),
    away_team_std = clean_team_name(Away),
    attendance    = as.numeric(FinalAttendance_R1)
  ) %>%
  select(season_std, home_team_std, away_team_std, attendance)

# 3. Left-join onto matches_clean
matches_clean <- matches_clean %>%
  mutate(
    season_std    = str_replace_all(season, "–|-", "/"),
    home_team_std = clean_team_name(home_team),
    away_team_std = clean_team_name(away_team)
  ) %>%
  left_join(
    szabo_wsl,
    by = c("season_std", "home_team_std", "away_team_std"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    # Create continuous and binary crowd variables (matching Szabo & Kerenyi, 2023)
    closed_doors   = if_else(!is.na(attendance) & attendance == 0, 1L, 0L),
    log_attendance = log(attendance + 1)
  ) %>%
  select(-season_std, -home_team_std, -away_team_std)

# 4. Verify match overlap and check for any unmatched team names
overlap_check <- matches_clean %>%
  filter(
    str_detect(competition, "FA Women's Super League"),
    str_replace_all(season, "–|-", "/") %in% c("2018/2019", "2019/2020", "2020/2021", "2021/2022")
  ) %>%
  summarise(
    overlap_rows       = n(),
    matched_attendance = sum(!is.na(attendance)),
    unmatched_rows     = sum(is.na(attendance)),
    zero_attendance    = sum(attendance == 0, na.rm = TRUE) # should be 128
  )

print(overlap_check)

# Inspect any unmatched rows (if unmatched_rows > 0)
unmatched <- matches_clean %>%
  filter(
    str_detect(competition, "FA Women's Super League"),
    str_replace_all(season, "–|-", "/") %in% c("2018/2019", "2019/2020", "2020/2021", "2021/2022"),
    is.na(attendance)
  ) %>%
  distinct(season, home_team, away_team)

if (nrow(unmatched) > 0) print(unmatched) ## attendance missing

# 5. Propagate attendance & closed_doors onto stacked_clean via match_id
stacked_clean <- stacked_clean %>%
  left_join(
    matches_clean %>% select(match_id, attendance, log_attendance, closed_doors),
    by = "match_id"
  )

# Save merged clean datasets
saveRDS(matches_clean, PROCESSED("statsbomb_matches_clean.rds"))
saveRDS(stacked_clean, PROCESSED("statsbomb_stacked_clean.rds"))