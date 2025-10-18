library(here)
library(dplyr)

voter_database <- readr::read_delim(
  here('data/20251001_VRDB_Extract.txt'),
  delim = '|'
) |>
  filter(CountyCode == 'KI') |>
  select(
    voter_id = StateVoterID,
    birth_year = Birthyear,
    gender = Gender,
    last_voted = LastVoted
  )

write.csv(
  voter_database,
  here('data/voter_database_summary.csv'),
  row.names = FALSE
)
