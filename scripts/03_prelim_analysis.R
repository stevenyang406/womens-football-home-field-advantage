source("000_header.R")

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

## Creating results table for comparison with existing literature
ha_overall <- matches_clean %>%
  summarise(
    Matches = n(),
    `Home Win %` = round(mean(home_win) * 100, 2),
    `Draw %` = round(mean(draw) * 100, 2),
    `Away Win %` = round(mean(away_win) * 100, 2),
    `Home Win % Exc Draws` = round(mean(home_win[draw == 0]) * 100, 2),
    `Pollard HA %` = round(sum(home_points) / sum(total_points) * 100, 2),
    `Avg Goal Diff` = round(mean(score_diff), 3),
    `Avg xG Diff` = round(mean(xg_diff, na.rm = TRUE), 3)
  )

ha_overall_display <- ha_overall %>%
  mutate(
    `Depken p-val` = round(t_depken$p.value, 4),
    `Jamieson p-val` = round(t_jamieson$p.value, 4),
    `Pollard & Gómez (2014) Benchmark` = "54.20%",
    `Depken & Globan (2025) Benchmark` = "52.30%",
    `Szabó & Kerényi (2023) Implied Exc Draws` = "55.26%" # 46.17 / (46.17 + 37.39)
  )