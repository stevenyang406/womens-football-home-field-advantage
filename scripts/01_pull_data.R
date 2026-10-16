comps <- FreeCompetitions()

target_comps <- comps %>%
  filter(
    competition_id %in% c(37, 49, 131, 135, 182),
    competition_gender == "female"
  )

matches_raw <- FreeMatches(target_comps)
events_raw <- free_allevents(MatchesDF = matches_raw, Parallel = TRUE)

saveRDS(events_raw, RAW("statsbomb_wsl_events_raw.rds"))
saveRDS(matches_raw, RAW("statsbomb_wsl_matches_raw.rds"))