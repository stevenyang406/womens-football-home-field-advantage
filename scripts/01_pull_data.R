events_file_loc  <- RAW("statsbomb_events_raw.rds")
matches_file_loc <- RAW("statsbomb_matches_raw.rds")

if (!file.exists(events_file_loc) || !file.exists(matches_file_loc)) {
  comps <- FreeCompetitions()

  target_comps <- comps %>%
    filter(
      competition_id %in% c(37, 49, 131, 135, 182),
      competition_gender == "female"
    )

  matches_raw <- FreeMatches(target_comps)
  events_raw <- free_allevents(MatchesDF = matches_raw, Parallel = TRUE)

  saveRDS(events_raw, events_file_loc)
  saveRDS(matches_raw, matches_file_loc)
} else {
  message("Raw file already exists.")
}