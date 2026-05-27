library(StatsBombR)
library(dplyr)

#pull the free competitions directory
free_comps <- FreeCompetitions()

#filter for La Liga (competition_id 11) using the season_name column with quotes
my_competitions <- free_comps %>%
  filter(competition_id == 11,
         season_name %in% c("2017/2018", "2018/2019", "2019/2020", "2020/2021"))

#download the match metadata for those seasons
my_matches <- FreeMatches(my_competitions)

#download all play-by-play events for those matches
my_events <- free_allevents(my_matches)

#filter for the specific injury stoppages and injury substitutions
injury_events <- my_events %>%
  filter(type.id == 40 | (type.id == 19 & substitution.outcome.id == 102))

#view final dataset
View(injury_events)