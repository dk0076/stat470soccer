library(StatsBombR)
library(dplyr)

# 1. Pull the free competitions directory
free_comps <- FreeCompetitions()

# 2. Filter for La Liga (competition_id 11) using the season_name column with quotes
my_competitions <- free_comps %>%
  filter(competition_id == 11,
         season_name %in% c("2017/2018", "2018/2019", "2019/2020", "2020/2021"))

# Download the match metadata for those seasons (includes match dates for your congestion predictor)
my_matches <- FreeMatches(my_competitions)

# 4. Download ALL play-by-play events for those matches
# (This might take a minute or two to run)
my_events <- free_allevents(my_matches)

# 5. Filter for the specific Injury Stoppages (40) and Injury Substitutions (19, outcome 102)
injury_events <- my_events %>%
  filter(type.id == 40 | (type.id == 19 & substitution.outcome.id == 102))

# View your final injury dataset!
View(injury_events)