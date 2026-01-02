# Test file for make_surv_dataset function

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
    nif::nif()
}

test_that("make_surv_dataset validates input parameters correctly", {
  # Create mock data
  mock_nif <- create_mock_nif_with_events()

  # Test error for non-nif object
  expect_error(
    make_surv_dataset(
      nif = data.frame(),
      analyte = "EV_HEADACHE"
    ),
    "Input must be a nif object"
  )

  # Test error for NULL analyte
  expect_error(
    make_surv_dataset(
      nif = mock_nif,
      analyte = NULL
    ),
    "analyte must be a single character string"
  )

  # Test error for non-character analyte
  expect_error(
    make_surv_dataset(
      nif = mock_nif,
      analyte = 123
    ),
    "analyte must be a single character string"
  )

  # Test error for invalid convert_tafd_h_to_d parameter
  expect_error(
    make_surv_dataset(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      convert_tafd_h_to_d = "not_logical"
    ),
    "convert_tafd_h_to_d must be a single logical value"
  )

  # Test error for invalid silent parameter
  expect_error(
    make_surv_dataset(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      silent = "not_logical"
    ),
    "silent must be a single logical value"
  )
})

test_that("make_surv_dataset validates required columns", {
  # Create nif object missing required columns
  incomplete_nif <- create_mock_nif_with_events() %>%
    select(-ANALYTE)

  expect_error(
    make_surv_dataset(
      nif = incomplete_nif,
      analyte = "EV_HEADACHE"
    ),
    "Required column ANALYTE not found in nif data set"
  )

  # Test missing multiple columns
  incomplete_nif2 <- create_mock_nif_with_events() %>%
    select(-ANALYTE, -DV)

  expect_error(
    make_surv_dataset(
      nif = incomplete_nif2,
      analyte = "EV_HEADACHE"
    ),
    "Missing essential fields in nif object: DV"
  )
})

test_that("make_surv_dataset validates analyte existence", {
  mock_nif <- create_mock_nif_with_events()

  # Test error for non-existent analyte
  expect_error(
    make_surv_dataset(
      nif = mock_nif,
      analyte = "NONEXISTENT"
    ),
    "No data found for analyte 'NONEXISTENT'"
  )
})

test_that("make_surv_dataset validates group variables", {
  mock_nif <- create_mock_nif_with_events()

  # Test error for non-existent group variable
  expect_error(
    make_surv_dataset(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      group = "NONEXISTENT_GROUP"
    ),
    "Grouping variable NONEXISTENT_GROUP not found in nif data set"
  )

  # Test error for multiple non-existent group variables
  expect_error(
    make_surv_dataset(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      group = c("NONEXISTENT1", "NONEXISTENT2")
    ),
    "Grouping variables NONEXISTENT1 and NONEXISTENT2 not found in nif data set"
  )

  # Test success with valid group variable
  expect_silent(
    make_surv_dataset(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      group = "SEX"
    )
  )
})

test_that("make_surv_dataset validates DV values", {
  mock_nif <- create_mock_nif_with_events()

  # Test error for DV with NA values
  na_dv_nif <- mock_nif %>%
    mutate(DV = ifelse(ANALYTE == "EV_HEADACHE" & TAFD == 6, NA, DV))

  expect_error(
    make_surv_dataset(
      nif = na_dv_nif,
      analyte = "EV_HEADACHE"
    ),
    "DV for the analyte cannot contain NA values"
  )

  # Test error for DV with invalid values
  invalid_dv_nif <- mock_nif %>%
    mutate(DV = ifelse(ANALYTE == "EV_HEADACHE" & TAFD == 6, 2, DV))

  expect_error(
    make_surv_dataset(
      nif = invalid_dv_nif,
      analyte = "EV_HEADACHE"
    ),
    "DV for the analyte must be either 0 or 1, or FALSE or TRUE"
  )

  # Test success with valid DV values (0, 1, FALSE, TRUE)
  valid_dv_nif <- mock_nif %>%
    mutate(DV = ifelse(ANALYTE == "EV_HEADACHE" & TAFD == 6, TRUE, DV))

  expect_silent(
    make_surv_dataset(
      nif = valid_dv_nif,
      analyte = "EV_HEADACHE"
    )
  )
})

test_that("make_surv_dataset handles TAFD filtering correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Test with negative TAFD values
  negative_tafd_nif <- mock_nif %>%
    mutate(TAFD = ifelse(ID == 1 & ANALYTE == "EV_HEADACHE" & TAFD == 6, -1, TAFD))

  expect_message(
    make_surv_dataset(
      nif = negative_tafd_nif,
      analyte = "EV_HEADACHE",
      silent = FALSE
    ),
    "1 row with negative TAFD removed from survival data set!"
  )

  # Test with multiple negative TAFD values
  multiple_negative_tafd_nif <- mock_nif %>%
    mutate(TAFD = ifelse(ANALYTE == "EV_HEADACHE" & TAFD %in% c(6, 8), -1, TAFD))

  expect_message(
    make_surv_dataset(
      nif = multiple_negative_tafd_nif,
      analyte = "EV_HEADACHE",
      silent = FALSE
    ),
    "3 rows with negative TAFD removed from survival data set!"
  )

  # Test with all negative TAFD values (should error)
  all_negative_tafd_nif <- mock_nif %>%
    mutate(TAFD = ifelse(ANALYTE == "EV_HEADACHE", -1, TAFD))

  expect_message(
    expect_error(
      make_surv_dataset(
        nif = all_negative_tafd_nif,
        analyte = "EV_HEADACHE",
        silent = FALSE
      ),
      "No event data found for analyte 'EV_HEADACHE'"
    ),
    "4 rows with negative TAFD removed from survival data set!"
  )
})

test_that("make_surv_dataset handles missing TAFD values", {
  mock_nif <- create_mock_nif_with_events()

  # Test error for missing TAFD values
  missing_tafd_nif <- mock_nif %>%
    mutate(TAFD = ifelse(ANALYTE == "EV_HEADACHE", NA, TAFD))

  expect_error(
    make_surv_dataset(
      nif = missing_tafd_nif,
      analyte = "EV_HEADACHE"
    ),
    "Missing time values found for events"
  )
})

test_that("make_surv_dataset handles time conversion correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Test with convert_tafd_h_to_d = TRUE (default)
  result1 <- make_surv_dataset(
    nif = mock_nif,
    analyte = "EV_HEADACHE",
    convert_tafd_h_to_d = TRUE
  )

  # Check that time values are converted from hours to days
  expect_true(all(result1$time <= max(mock_nif$TAFD) / 24))

  # Test with convert_tafd_h_to_d = FALSE
  result2 <- make_surv_dataset(
    nif = mock_nif,
    analyte = "EV_HEADACHE",
    convert_tafd_h_to_d = FALSE
  )

  # Check that time values are not converted
  expect_true(all(result2$time <= max(mock_nif$TAFD)))
})

test_that("make_surv_dataset returns correct output structure", {
  mock_nif <- create_mock_nif_with_events()

  # Test without grouping
  result1 <- make_surv_dataset(
    nif = mock_nif,
    analyte = "EV_HEADACHE"
  )

  # Check output structure
  expect_true(is.data.frame(result1))
  expect_equal(names(result1), c("ID", "time", "status"))
  expect_true(all(c("time", "status") %in% names(result1)))

  # Test with grouping
  result2 <- make_surv_dataset(
    nif = mock_nif,
    analyte = "EV_HEADACHE",
    group = "SEX"
  )

  # Check output structure with grouping
  expect_true(is.data.frame(result2))
  expect_true(all(c("ID", "SEX", "time", "status") %in% names(result2)))
})

test_that("make_surv_dataset calculates survival data correctly", {
  mock_nif <- create_mock_nif_with_events()

  result <- make_surv_dataset(
    nif = mock_nif,
    analyte = "EV_HEADACHE"
  )

  # Check that we have the expected number of subjects
  expect_equal(nrow(result), 2)  # Two subjects in mock data

  # Check that status is binary (0 or 1)
  expect_true(all(result$status %in% c(0, 1)))

  # Check that time values are positive
  expect_true(all(result$time >= 0))
})

test_that("make_surv_dataset handles no events correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Create data with no events (all DV = 0)
  no_events_nif <- mock_nif %>%
    mutate(DV = ifelse(ANALYTE == "EV_HEADACHE", 0, DV))

  expect_message(
    result <- make_surv_dataset(
      nif = no_events_nif,
      analyte = "EV_HEADACHE",
      silent = FALSE
    ),
    "No events found for analyte 'EV_HEADACHE'"
  )

  # Should still return data but with warning
  expect_message(
    make_surv_dataset(
      nif = no_events_nif,
      analyte = "EV_HEADACHE",
      silent = FALSE
    ),
    "No events found for analyte 'EV_HEADACHE'"
  )

  # Check that all status values are 0 (no events)
  expect_true(all(result$status == 0))
})

test_that("make_surv_dataset handles all events correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Create data with all events (all DV = 1)
  all_events_nif <- mock_nif %>%
    mutate(DV = ifelse(ANALYTE == "EV_HEADACHE", 1, DV))

  result <- make_surv_dataset(
    nif = all_events_nif,
    analyte = "EV_HEADACHE"
  )

  # Check that all status values are 1 (all events)
  expect_true(all(result$status == 1))
})

test_that("make_surv_dataset handles multiple group variables", {
  mock_nif <- create_mock_nif_with_events() %>%
    mutate(AGE_GROUP = ifelse(SEX == 1, "YOUNG", "OLD"))

  result <- make_surv_dataset(
    nif = mock_nif,
    analyte = "EV_HEADACHE",
    group = c("SEX", "AGE_GROUP")
  )

  # Check that all group variables are included in output
  expect_true(all(c("SEX", "AGE_GROUP") %in% names(result)))
  expect_true(all(c("ID", "SEX", "AGE_GROUP", "time", "status") %in% names(result)))
})

test_that("make_surv_dataset handles edge cases", {
  mock_nif <- create_mock_nif_with_events()

  # Test with single subject
  single_subject_nif <- mock_nif %>%
    filter(USUBJID == "001")

  result <- make_surv_dataset(
    nif = single_subject_nif,
    analyte = "EV_HEADACHE"
  )

  expect_equal(nrow(result), 1)

  # Test with subjects having same event times
  same_time_nif <- mock_nif %>%
    mutate(TAFD = ifelse(ANALYTE == "EV_HEADACHE", 6, TAFD))

  result2 <- make_surv_dataset(
    nif = same_time_nif,
    analyte = "EV_HEADACHE"
  )

  expect_equal(nrow(result2), 2)
})

test_that("make_surv_dataset handles silent parameter correctly", {
  mock_nif <- create_mock_nif_with_events()

  # Test with silent = TRUE (should suppress messages)
  expect_silent(
    make_surv_dataset(
      nif = mock_nif,
      analyte = "EV_HEADACHE",
      silent = TRUE
    )
  )

  # Test with silent = FALSE (should show messages)
  negative_tafd_nif <- mock_nif %>%
    mutate(TAFD = ifelse(ANALYTE == "EV_HEADACHE" & TAFD == 6, -1, TAFD))

  expect_message(
    make_surv_dataset(
      nif = negative_tafd_nif,
      analyte = "EV_HEADACHE",
      silent = FALSE
    ),
    "2 rows with negative TAFD removed from survival data set!"
  )
})

