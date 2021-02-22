# import package
library(magrittr)

# load raw data
data <- readr::read_rds('data-raw/data.rds.xz')

# drop irrelevant columns and rename remaining columns
data <- data %>%
  dplyr::select(-dplyr::matches('dataset|recordid.y|record_timestamp.y')) %>%
  janitor::clean_names() %>%
  dplyr::rename_all(~stringr::str_remove_all(., '_[xy]{1}\\b'))

data <- data %>%
  dplyr::mutate(
    record_timestamp = lubridate::ymd_hms(.data$record_timestamp)
  ) %>%
  dplyr::mutate_at(
    dplyr::vars(dplyr::matches('\\b(cat|file|juris|period|case)')), as.factor
  ) %>%
  dplyr::mutate(
    ln_number_of_cases = number_of_cases %>% {ifelse(. > 0, log(.), log(1))},
    period = ordered(.data$period, levels = sort(unique(.data$period)))
  ) %>%
  dplyr::select(
    .data$period, .data$year, .data$county, 1:5, dplyr::matches('case_'),
    dplyr::matches('_case'), .data$population
  )

# save the processed data
usethis::use_data(data, overwrite = TRUE)

