# Test file for kmplot function
# Helper function to create mock nif object with events
create_mock_nif_with_events <- function() {
  tibble::tribble(
    ~USUBJID,                  ~DTC,      ~ANALYTE, ~DV, ~TIME, ~CMT, ~AMT, ~DOSE, ~PARENT, ~METABOLITE, ~EVID, ~MDV, ~IMPUTATION, ~TAFD, ~ID, ~SEX,
    "001", "2023-01-01 08:00:00",        "DRUG",   0,     0,    1,  100,   100,  "DRUG",       FALSE,     1,    0,          NA,     0,     1,   1,
    "001", "2023-01-01 10:00:00",        "DRUG",  50,     2,    2,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     2,     1,   1,
    "001", "2023-01-01 12:00:00",        "DRUG",  25,     4,    2,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     4,     1,   1,
    "001", "2023-01-01 14:00:00", "EV_HEADACHE",   1,     6,    3,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     6,     1,   1,
    "001", "2023-01-01 16:00:00", "EV_HEADACHE",   0,     8,    3,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     8,     1,   1,
    "002", "2023-01-01 08:00:00",        "DRUG",   0,     0,    1,  100,   100,  "DRUG",       FALSE,     1,    0,          NA,     0,     2,   0,
    "002", "2023-01-01 10:00:00",        "DRUG",  45,     2,    2,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     2,     2,   0,
    "002", "2023-01-01 12:00:00", "EV_HEADACHE",   1,     4,    3,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     4,     2,   0,
    "002", "2023-01-01 14:00:00", "EV_HEADACHE",   0,     6,    3,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     6,     2,   0
  ) %>%
    mutate(
      DTC = as.POSIXct(DTC),
      RFSTDTC = as.POSIXct("2023-01-01 08:00:00"),
      TRTDY = 1,
      TAFD = as.numeric(difftime(DTC, RFSTDTC, units = "hours"))
    ) %>%
    nif::new_nif()
}

test_that("kmplot validates input parameters correctly", {
  # Create mock data
  mock_nif <- create_mock_nif_with_events()

  # Test error for non-nif object
  expect_error(
    kmplot(
      nif = data.frame(),
      analyte = "EV_HEADACHE"
    ),
    "Input must be a NIF object"
  )

  # Test error for missing analyte
  expect_error(
    kmplot(
      nif = mock_nif,
      analyte = NULL
    ),
    "analyte must be a single character string"
  )

  # Test error for non-existent analyte
  expect_error(
    kmplot(
      nif = mock_nif,
      analyte = "NONEXISTENT"
    ),
    "No data found for analyte 'NONEXISTENT'"
  )
})

test_that("kmplot handles empty data sets", {
  # Create empty nif object
  empty_nif <- create_mock_nif_with_events()[0, ]
  class(empty_nif) <- c("nif", "data.frame")

  # Test with empty nif
  expect_error(
    kmplot(
      nif = empty_nif,
      analyte = "EV_HEADACHE"
    ),
    "No data found for analyte 'EV_HEADACHE'"
  )

  # Test with nif that has no events for the analyte
  no_events_nif <- create_mock_nif_with_events() %>%
    filter(ANALYTE != "EV_HEADACHE")

  expect_error(
    kmplot(
      nif = no_events_nif,
      analyte = "EV_HEADACHE"
    ),
    "No data found for analyte 'EV_HEADACHE'"
  )
})

test_that("kmplot handles dose filtering correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Test with specific dose
  expect_error(
    kmplot(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      dose = 999  # Non-existent dose
    ),
    "No data found for specified dose: 999"
  )

  # Test with multiple doses
  multi_dose_nif <- rbind(
    create_mock_nif_with_events(),
    create_mock_nif_with_events() %>% mutate(DOSE = 200)
  )

  # Should work with multiple doses
  # expect_silent(
  expect_no_error(
    kmplot(
      nif = multi_dose_nif,
      analyte = "EV_HEADACHE"
    )
  )
})

test_that("kmplot handles grouping correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Test with non-existent group variable
  expect_error(
    kmplot(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      group = "NONEXISTENT_GROUP"
    ),
    "Grouping variable NONEXISTENT_GROUP not found in nif data set"
  )

  # Test with valid group variable
  expect_silent(
    kmplot(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      group = "SEX"
    )
  )
})

test_that("kmplot handles edge cases in survival data", {
  mock_nif <- create_mock_nif_with_events()

  # Test with no events (all status = 0)
  no_events_nif <- mock_nif %>%
    mutate(DV = ifelse(ANALYTE == "EV_HEADACHE", 0, DV))

  expect_message(
    kmplot(
      nif = no_events_nif,
      analyte = "EV_HEADACHE",
      silent = FALSE
    ),
    "No events found for analyte 'EV_HEADACHE'"
  )

  # Test with all events (all status = 1)
  all_events_nif <- mock_nif %>%
    mutate(DV = ifelse(ANALYTE == "EV_HEADACHE", 1, DV))

  expect_silent(
    kmplot(
      nif = all_events_nif,
      analyte = "EV_HEADACHE"
    )
  )
})

test_that("kmplot handles time calculations correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Test with negative TAFD values
  negative_time_nif <- mock_nif %>%
    # mutate(TAFD = ifelse(ANALYTE == "EV_HEADACHE", -1, TAFD))
    mutate(TAFD = ifelse(ANALYTE == "EV_HEADACHE" & TAFD == 4, -1, TAFD))

  expect_message(
    kmplot(
      nif = negative_time_nif,
      analyte = "EV_HEADACHE",
      silent = FALSE
    ),
    "1 row with negative TAFD removed from survival data set!"
  )

  # Test with missing TAFD values
  missing_time_nif <- mock_nif %>%
    mutate(TAFD = ifelse(ANALYTE == "EV_HEADACHE", NA, TAFD))

  expect_error(
    kmplot(
      nif = missing_time_nif,
      analyte = "EV_HEADACHE"
    ),
    "Missing time values found for events"
  )
})

test_that("kmplot validates additional parameters", {
  mock_nif <- create_mock_nif_with_events()

  # Test convert_tafd_h_to_d parameter validation
  expect_error(
    kmplot(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      convert_tafd_h_to_d = "not_logical"
    ),
    "convert_tafd_h_to_d must be a single logical value"
  )

  # Test silent parameter validation
  expect_error(
    kmplot(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      silent = "not_logical"
    ),
    "silent must be a single logical value"
  )

  # Test title parameter validation
  expect_error(
    kmplot(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      title = 123
    ),
    "title must be a single character string"
  )

  # Test y_label parameter validation
  expect_error(
    kmplot(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      y_label = 123
    ),
    "y_label must be a single character string"
  )
})

test_that("kmplot handles convert_tafd_h_to_d parameter correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Test with convert_tafd_h_to_d = TRUE (default)
  result1 <- kmplot(
    nif = mock_nif,
    analyte = "EV_HEADACHE",
    convert_tafd_h_to_d = TRUE
  )
  expect_true(inherits(result1, "ggsurvfit"))

  # Test with convert_tafd_h_to_d = FALSE
  result2 <- kmplot(
    nif = mock_nif,
    analyte = "EV_HEADACHE",
    convert_tafd_h_to_d = FALSE
  )
  expect_true(inherits(result2, "ggsurvfit"))
})

test_that("kmplot handles edge cases with survival analysis", {
  mock_nif <- create_mock_nif_with_events()

  # Test with single subject
  single_subject_nif <- mock_nif %>%
    filter(USUBJID == "001")

  expect_silent(
    kmplot(
      nif = single_subject_nif,
      analyte = "EV_HEADACHE"
    )
  )

  # Test with all subjects having same event time
  same_time_nif <- mock_nif %>%
    mutate(TAFD = ifelse(ANALYTE == "EV_HEADACHE", 6, TAFD))

  expect_silent(
    kmplot(
      nif = same_time_nif,
      analyte = "EV_HEADACHE"
    )
  )
})

test_that("kmplot handles survminer parameters correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Test with risk table
  expect_silent(
    kmplot(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      risk.table = TRUE
    )
  )
})

test_that("kmplot returns correct object structure", {
  mock_nif <- create_mock_nif_with_events()

  # Test that function returns a ggplot object
  result <- kmplot(
    nif = mock_nif,
    analyte = "EV_HEADACHE"
  )

  # Check that result is a survminer object (not ggplot directly)
  expect_true(inherits(result, "ggplot"))
  expect_true(inherits(result, "ggsurvfit"))
})

test_that("kmplot handles legend and labels correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Test with custom title and y_label
  result <- kmplot(
    nif = mock_nif,
    analyte = "EV_HEADACHE",
    title = "Custom Title",
    y_label = "Custom Label"
  )

  expect_true(inherits(result, "ggsurvfit"))

  # Test with NULL title and y_label
  result2 <- kmplot(
    nif = mock_nif,
    analyte = "EV_HEADACHE",
    title = NULL,
    y_label = NULL
  )

  expect_true(inherits(result2, "ggsurvfit"))
})

test_that("kmplot handles data with only one group", {
  mock_nif <- create_mock_nif_with_events()

  # Test with grouping but only one group value
  single_group_nif <- mock_nif %>%
    mutate(SEX = 1)  # All subjects have same group

  expect_silent(
    kmplot(
      nif = single_group_nif,
      analyte = "EV_HEADACHE",
      group = "SEX"
    )
  )
})

test_that("kmplot handles data with missing values in group variable", {
  mock_nif <- create_mock_nif_with_events()

  # Test with missing values in group variable
  missing_group_nif <- mock_nif %>%
    mutate(SEX = ifelse(USUBJID == "001", NA, SEX))

  expect_silent(
    kmplot(
      nif = missing_group_nif,
      analyte = "EV_HEADACHE",
      group = "SEX"
    )
  )
})

