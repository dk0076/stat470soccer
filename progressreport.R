library(worldfootballR)
library(tidyverse)

# ── 1. PULL TEAM URLS FOR BOTH SEASONS ───────────────────────────────────────

team_urls_2023 <- tm_league_team_urls(
  league_url = "https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1",
  start_year = 2023
)

team_urls_2024 <- tm_league_team_urls(
  league_url = "https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1",
  start_year = 2024
)

# ── 2. PULL SQUAD STATS FOR BOTH SEASONS ─────────────────────────────────────

squad_2023 <- map_dfr(team_urls_2023, function(url) {
  Sys.sleep(2)
  tryCatch(tm_squad_stats(team_url = url), error = function(e) NULL)
}) %>% mutate(season = "2023/2024")

squad_2024 <- map_dfr(team_urls_2024, function(url) {
  Sys.sleep(2)
  tryCatch(tm_squad_stats(team_url = url), error = function(e) NULL)
}) %>% mutate(season = "2024/2025")

squad_all <- bind_rows(squad_2023, squad_2024)
saveRDS(squad_all, "pl_squad_all.rds")

squad_all %>% distinct(player_pos)

# ── 1. FILTER ATTACKERS ───────────────────────────────────────────────────────

squad_attackers <- squad_all %>%
  filter(league == "Premier League") %>%
  filter(player_pos %in% c("Centre-Forward", "Left Winger", "Right Winger",
                           "Attacking Midfield")) %>%
  mutate(
    goals_per_app   = if_else(appearances > 0, goals / appearances, NA_real_),
    congestion_rate = if_else(in_squad > 0, appearances / in_squad, NA_real_),
    mins_per_app    = minutes_played / appearances,
  ) %>%
  filter(appearances >= 5) %>%
  mutate(
    high_congestion = if_else(congestion_rate >= quantile(congestion_rate, 0.75, na.rm = TRUE), 1, 0)
  )

# did not include second striker as only 3 were listed

squad_attackers %>% count(season)
squad_attackers %>% distinct(league)


# ── 2. BUILD PERFORMANCE METRIC ───────────────────────────────────────────────

squad_attackers <- squad_attackers %>%
  mutate(
    # Remove players with very few appearances to avoid noise
    appearances = as.numeric(appearances),
    minutes_played = as.numeric(minutes_played),
    in_squad = as.numeric(in_squad),
    goals = as.numeric(goals),
    
    # Goals per appearance (performance metric)
    goals_per_app = if_else(appearances > 0, goals / appearances, NA_real_),
    
    # Congestion rate (how often they played when available)
    congestion_rate = if_else(in_squad > 0, appearances / in_squad, NA_real_)
  ) %>%
  # Filter out players with too few appearances to be meaningful
  filter(appearances >= 5)

# ── 3. CHECK DATA BEFORE MODELING ─────────────────────────────────────────────

glimpse(squad_attackers)
summary(squad_attackers$goals_per_app)
summary(squad_attackers$congestion_rate)
squad_attackers %>% count(season)

# ── 4. MODELS ─────────────────────────────────────────────────────────────────

# Linear regression: effect of congestion on goal contributions per appearance
lm_model <- lm(
  goals_per_app ~ congestion_rate + player_age + season,
  data = squad_attackers
)
summary(lm_model)

# Check assumptions
par(mfrow = c(2,2))
plot(lm_model)

# ── 5. SAVE ───────────────────────────────────────────────────────────────────

saveRDS(squad_attackers, "pl_attackers_final.rds")



library(sandwich)
library(lmtest)
coeftest(lm_model, vcov = vcovHC(lm_model, type = "HC3"))
squad_attackers[c(4, 184, 214), ] %>% 
  select(player_name, team_name, season, goals, appearances, goals_per_app, congestion_rate)


# Model 2: high congestion binary flag 
# squad_attackers <- squad_attackers %>%
#   mutate(high_congestion = if_else(congestion_rate >= quantile(congestion_rate, 0.75), 1, 0))
# 
# lm_model2 <- lm(goals_per_app ~ high_congestion + player_age + season, 
#                 data = squad_attackers)
# summary(lm_model2)

# Model 3: adding minutes per appearance as control
squad_attackers <- squad_attackers %>%
  mutate(mins_per_app = minutes_played / appearances)

lm_model3 <- lm(goals_per_app ~ congestion_rate + mins_per_app + player_age + season,
                data = squad_attackers)
summary(lm_model3)

summary(squad_attackers %>% select(goals_per_app, congestion_rate, mins_per_app, player_age, season))

library(ggplot2)

# ── 1. CONGESTION RATE VS GOALS PER APP (with Haaland & Salah labeled) ────────

labeled_players <- squad_attackers %>%
  filter(player_name %in% c("Erling Haaland", "Mohamed Salah"))

ggplot(squad_attackers, aes(x = congestion_rate, y = goals_per_app)) +
  geom_point(aes(color = player_pos), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  geom_label(data = labeled_players,
             aes(label = paste(player_name, season)),
             size = 3, nudge_y = 0.03) +
  labs(title = "Match Congestion vs Goal Contributions per Appearance",
       subtitle = "Premier League Attackers, 2023/24 and 2024/25",
       x = "Congestion Rate (Appearances / Squad Selections)",
       y = "Goals per Appearance",
       color = "Position") +
  theme_minimal()

# ── 2. MINS PER APP VS GOALS PER APP (the real driver) ───────────────────────

ggplot(squad_attackers, aes(x = mins_per_app, y = goals_per_app)) +
  geom_point(aes(color = player_pos), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  geom_label(data = labeled_players,
             aes(label = paste(player_name, season)),
             size = 3, nudge_y = 0.03) +
  labs(title = "Minutes per Appearance vs Goal Contributions",
       subtitle = "Premier League Attackers, 2023/24 and 2024/25",
       x = "Minutes per Appearance",
       y = "Goals per Appearance",
       color = "Position") +
  theme_minimal()

# ── 3. BOX PLOT: HIGH VS LOW CONGESTION ───────────────────────────────────────

# squad_attackers %>%
#   mutate(congestion_group = if_else(high_congestion == 1, 
#                                     "High Congestion (Top 25%)", 
#                                     "Lower Congestion")) %>%
#   ggplot(aes(x = congestion_group, y = goals_per_app, fill = congestion_group)) +
#   geom_boxplot(alpha = 0.7, outlier.shape = 21) +
#   geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
#   labs(title = "Goal Contributions by Congestion Group",
#        subtitle = "Premier League Attackers, 2023/24 and 2024/25",
#        x = "",
#        y = "Goals per Appearance") +
#   theme_minimal() +
#   theme(legend.position = "none")

# ── 4. CONGESTION RATE DISTRIBUTION BY SEASON ─────────────────────────────────

# ggplot(squad_attackers, aes(x = congestion_rate, fill = season)) +
#   geom_density(alpha = 0.5) +
#   labs(title = "Distribution of Congestion Rate by Season",
#        subtitle = "Premier League Attackers, 2023/24 and 2024/25",
#        x = "Congestion Rate",
#        y = "Density",
#        fill = "Season") +
#   theme_minimal()

# Partial regression plot for congestion after controlling for mins_per_app
squad_attackers <- squad_attackers %>%
  mutate(
    resid_goals = residuals(lm(goals_per_app ~ mins_per_app + player_age + season, 
                               data = squad_attackers)),
    resid_congestion = residuals(lm(congestion_rate ~ mins_per_app + player_age + season, 
                                    data = squad_attackers))
  )

ggplot(squad_attackers, aes(x = resid_congestion, y = resid_goals)) +
  geom_point(aes(color = player_pos), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  labs(title = "Partial Regression: Congestion Effect After Controlling for Minutes per Appearance",
       subtitle = "Flat slope confirms congestion has no independent effect on goal contributions",
       x = "Congestion Rate (residualized)",
       y = "Goals per Appearance (residualized)",
       color = "Position") +
  theme_minimal()


# ── 5. POSITION-STRATIFIED MODELS ────────────────────────────────────────────

positions <- unique(squad_attackers$player_pos)

position_models <- squad_attackers %>%
  group_by(player_pos) %>%
  group_map(~ {
    # Need at least 20 observations for a reliable model
    if (nrow(.x) < 20) return(NULL)
    lm(goals_per_app ~ congestion_rate + mins_per_app + player_age + season,
       data = .x)
  }, .keep = TRUE)

# Name the models by position
names(position_models) <- squad_attackers %>%
  group_by(player_pos) %>%
  group_keys() %>%
  pull(player_pos)

# View results for each position
position_results <- map2(position_models, names(position_models), function(model, name) {
  if (is.null(model)) return(NULL)
  cat("\n══════════════════════════════════\n")
  cat("Position:", name, "\n")
  cat("══════════════════════════════════\n")
  print(summary(model))
})

# ── 6. INTERACTION MODEL ──────────────────────────────────────────────────────

lm_model4 <- lm(
  goals_per_app ~ congestion_rate * mins_per_app + player_age + season,
  data = squad_attackers
)
summary(lm_model4)

# Visualize the interaction effect
# Create congestion groups for plotting
squad_attackers <- squad_attackers %>%
  mutate(congestion_group = case_when(
    congestion_rate >= quantile(congestion_rate, 0.75) ~ "High Congestion",
    congestion_rate <= quantile(congestion_rate, 0.25) ~ "Low Congestion",
    TRUE ~ "Medium Congestion"
  ))

ggplot(squad_attackers %>% filter(congestion_group != "Medium Congestion"),
       aes(x = mins_per_app, y = goals_per_app, color = congestion_group)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Interaction: Minutes per Appearance and Congestion on Goal Output",
       subtitle = "Comparing high vs low congestion players",
       x = "Minutes per Appearance",
       y = "Goals per Appearance",
       color = "Congestion Group") +
  theme_minimal()

# ── 7. LAGGED CONGESTION (partial reverse causality fix) ─────────────────────

# Create lagged congestion: does 2023/24 congestion predict 2024/25 output?
squad_attackers_deduped <- squad_attackers %>%
  group_by(player_url, player_name, player_pos, player_age, season) %>%
  summarise(
    goals_per_app   = sum(goals) / sum(appearances),
    congestion_rate = sum(appearances) / sum(in_squad),
    mins_per_app    = sum(minutes_played) / sum(appearances),
    .groups = "drop"
  )

lagged_data <- squad_attackers_deduped %>%
  select(player_url, season, congestion_rate, goals_per_app, 
         mins_per_app, player_age, player_pos) %>%
  pivot_wider(
    id_cols = c(player_url, player_pos),
    names_from = season,
    values_from = c(congestion_rate, goals_per_app, mins_per_app, player_age)
  ) %>%
  # Rename for clarity
  rename(
    congestion_2324  = `congestion_rate_2023/2024`,
    congestion_2425  = `congestion_rate_2024/2025`,
    goals_2324       = `goals_per_app_2023/2024`,
    goals_2425       = `goals_per_app_2024/2025`,
    mins_2324        = `mins_per_app_2023/2024`,
    mins_2425        = `mins_per_app_2024/2025`,
    age_2425         = `player_age_2024/2025`
  ) %>%
  # Only keep players who appeared in both seasons
  filter(!is.na(congestion_2324) & !is.na(goals_2425))

# Lagged model: does last season's congestion predict this season's output?
lm_lagged <- lm(
  goals_2425 ~ congestion_2324 + mins_2425 + age_2425 + player_pos,
  data = lagged_data
)
summary(lm_lagged)

# Scatter of 2023/24 congestion vs 2024/25 goals
ggplot(lagged_data, aes(x = congestion_2324, y = goals_2425)) +
  geom_point(aes(color = player_pos), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  labs(title = "Prior Season Congestion vs Following Season Goal Output",
       subtitle = "Players appearing in both 2023/24 and 2024/25",
       x = "Congestion Rate 2023/24",
       y = "Goals per Appearance 2024/25",
       color = "Position") +
  theme_minimal()

# How many players appear in both seasons?
cat("Players in both seasons:", nrow(lagged_data), "\n")

# ── 8. VISUALIZE POSITION-STRATIFIED RESULTS ─────────────────────────────────

# Coefficient plot comparing congestion effect across positions
position_coefs <- map2_dfr(position_models, names(position_models), function(model, name) {
  if (is.null(model)) return(NULL)
  coef_df <- as.data.frame(summary(model)$coefficients)
  coef_df$term <- rownames(coef_df)
  coef_df$position <- name
  coef_df
}) %>%
  filter(term == "congestion_rate") %>%
  rename(estimate = Estimate, se = `Std. Error`, p = `Pr(>|t|)`) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    significant = if_else(p < 0.05, "Significant", "Not Significant")
  )

ggplot(position_coefs, aes(x = reorder(position, estimate), 
                           y = estimate, color = significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_flip() +
  labs(title = "Congestion Rate Effect on Goals per Appearance by Position",
       subtitle = "Coefficients from position-stratified Model 3 (controlling for mins/app, age, season)",
       x = "",
       y = "Estimated Effect of Congestion Rate",
       color = "") +
  theme_minimal()

# Second Striker excluded from position models (n = 4, insufficient for reliable estimation)

# Save everything
saveRDS(lagged_data,       "pl_lagged_data.rds")
saveRDS(position_models,   "pl_position_models.rds")
saveRDS(lm_model4,         "pl_interaction_model.rds")
saveRDS(lm_lagged,         "pl_lagged_model.rds")

