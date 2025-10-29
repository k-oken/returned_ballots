if (requireNamespace("renv", quietly = TRUE)) {
  # This finds the renv project root, even if the .qmd is in a subdirectory
  root <- renv::activate(
    project = rprojroot::find_root(rprojroot::has_file("renv.lock"))
  )
  # Explicitly load the environment
  renv::load(project = root)
}

library(here)
library(dplyr)
library(sf)

i_am('website/prepare.R')

today <- Sys.Date()
run_success <- FALSE
n_run <- 0
# this should bump back the date if it is having trouble downloading (i.e., date is wrong)
while (!run_success & n_run <= 3) {
  download.file(
    paste0(
      'https://www.sos.wa.gov/sites/default/files/current_election/Statewide',
      today,
      '.zip'
    ),
    destfile = here('data/daily_returns.zip'),
    method = "curl"
  )
  if (file.size(here('data/daily_returns.zip')) > 200000) {
    run_success <- TRUE
  } else {
    n_run <- n_run + 1
    today <- today - 1
  }
}

unzip(zipfile = here('data/daily_returns.zip'), exdir = here('data'))

# read in data

obs_returns_tmp <- read.csv(here(
  'data',
  paste0('Ballot Status Report ', today, '.csv')
))
obs_returns <- obs_returns_tmp[-ncol(obs_returns_tmp)] |>
  `names<-`(names(obs_returns_tmp)[-1]) |>
  as_tibble() |>
  rename_all(
    ~ stringr::str_replace(
      stringr::str_to_lower(.),
      pattern = '\\.',
      replacement = '_'
    )
  ) |>
  mutate(ballot_id = row.names(obs_returns_tmp)) |>
  relocate(ballot_id, .before = voter_id) |>
  filter(stringr::str_detect(precinct, 'SEA')) |>
  mutate(
    received_date = lubridate::mdy_hms(received_date), # time stamp doesn't work, oh well.
    voter_id = as.integer(voter_id)
  ) |>
  select(-party)

primary_returns <- readr::read_csv(
  'https://www.sos.wa.gov/sites/default/files/past_elections/2025-08/King.csv',
  col_types = 'cicccccccccccicdccc',
  col_select = -Party
) |>
  rename_all(
    ~ stringr::str_replace(
      stringr::str_to_lower(.),
      pattern = ' ',
      replacement = '_'
    )
  ) |>
  filter(stringr::str_detect(precinct, 'SEA')) |>
  select(voter_id, election, ballot_status)

# Read predicted returns
predicted_returns <- readr::read_csv(here(
  'data/turnout_propensity_10_02_2025.csv'
))

# Read in voter database mainly to get age and when they last voted
voter_database <- readr::read_csv(here('data/voter_database_summary.csv'))

# Read in precinct-level demographics
demographics <- sf::st_read(
  here(
    'data/precinct_demographics_09_04_2025.gpkg'
  ),
  quiet = TRUE
) |>
  rename_all(
    ~ stringr::str_replace(
      stringr::str_to_lower(.),
      pattern = ' ',
      replacement = '_'
    )
  ) |>
  filter(stringr::str_detect(name, 'SEA')) |>
  mutate(precinct_code = stringr::str_extract(name, '(?<=-)[:digit:]+'))

# read in primary results (not turnout)
primary <- readr::read_csv(here(
  'data/final-results-report-with-districts.csv'
)) |>
  filter(Race == 'City of Seattle Mayor') |>
  group_by(Precinct) |>
  summarize(
    pct_wilson = sum(SumOfCount * (CounterType == 'Katie Wilson')) /
      sum(SumOfCount * (CounterType == 'Times Counted'))
  )

# Join data frames for voter level return data
voter_returns <- predicted_returns |>
  select(voter_id, p_turnout_raw, turnout_flag_raw, precinct4) |>
  left_join(
    select(obs_returns, -c(county:election, address:country))
  ) |>
  left_join(primary_returns, by = 'voter_id', suffix = c('', '_primary')) |>
  left_join(voter_database) |>
  mutate(
    voter_id = as.integer(voter_id),
    birth_year = as.integer(birth_year),
    age = as.integer(2025 - birth_year)
  )

# Summarize to precinct level, then join with demographic data
precinct_returns_tmp <- voter_returns |>
  group_by(precinct4) |>
  summarise(
    obs_turnout = sum(ballot_status == 'Accepted', na.rm = TRUE) / n(),
    exp_turnout = mean(p_turnout_raw, na.rm = TRUE),
    primary_turnout = sum(ballot_status_primary == 'Accepted', na.rm = TRUE) /
      n(),
    avg_age = mean(age, na.rm = TRUE),
    # there are some voters not in my voter database that are in the predicted turnout
    avg_age_voted = sum(age * (ballot_status == 'Accepted'), na.rm = TRUE) /
      sum(ballot_status == 'Accepted', na.rm = TRUE),
    n_reg_voters = n()
  ) |>
  mutate(
    pct_of_expected = obs_turnout / exp_turnout,
    pct_of_primary = obs_turnout / primary_turnout
  )
# there are some precincts in predicted turnout that don't show up in demographics
# also some precincts in demographics that seem to not be in Seattle
# therefore, useing inner_join
precinct_returns <- inner_join(
  demographics,
  precinct_returns_tmp,
  by = c(precinct_code = 'precinct4')
) |>
  left_join(primary, by = c(name = 'Precinct'))

if (!exists(here('cache'))) {
  dir.create(here('cache'))
}

save(
  precinct_returns,
  voter_returns,
  today,
  file = here('cache/shared_objects.RData')
)
