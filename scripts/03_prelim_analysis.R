matches_clean <- readRDS(PROCESSED("statsbomb_matches_clean.rds"))
stacked_clean <- readRDS(PROCESSED("statsbomb_stacked_clean.rds"))

## Home win percentages (Depken, Jamieson)

home_win_prop_total <- sum(matches_clean$home_win) / nrow(matches_clean)
decisive <- matches_clean %>% filter(draw == 0)
home_win_prop_excl_draws <- sum(decisive$home_win) / nrow(decisive)

### t-test for Depken
t_depken <- t.test(matches_clean$home_win, mu = 0.5, alternative = "greater")

### Binomial and t-test for home win percentages (Jamieson)
binomial_jamieson <- binom.test(sum(decisive$home_win), nrow(decisive), p = 0.5)
t_jamieson <- t.test(decisive$home_win, mu = 0.5)

## Home points % (Pollard)
points_won_prop <- sum(matches_clean$home_points) / sum(matches_clean$total_points)