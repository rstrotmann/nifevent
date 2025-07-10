# Test file for add_event_observation function
# Helper function to create mock nif object
create_mock_nif <- function() {
  tibble::tribble(
    ~USUBJID,                  ~DTC, ~ANALYTE, ~DV, ~TIME, ~CMT, ~AMT, ~DOSE, ~PARENT, ~METABOLITE, ~EVID, ~MDV, ~IMPUTATION, ~TAFD, ~ID,
       "001", "2023-01-01 08:00:00",   "DRUG",   0,     0,    1,  100,   100,  "DRUG",       FALSE,     1,    0,          NA,     0,   1,
       "001", "2023-01-01 10:00:00",   "DRUG",  50,     2,    1,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     2,   1,
       "001", "2023-01-01 12:00:00",   "DRUG",  25,     4,    1,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     4,   1,
       "002", "2023-01-01 08:00:00",   "DRUG",   0,     0,    1,  100,   100,  "DRUG",       FALSE,     1,    0,          NA,     0,   2,
       "002", "2023-01-01 10:00:00",   "DRUG",  45,     2,    1,    0,   100,  "DRUG",       FALSE,     0,    0,          NA,     2,   2
  ) %>%
    mutate(
      DTC = as.POSIXct(DTC),
      RFSTDTC = as.POSIXct("2023-01-01 08:00:00"),
      TRTDY = 1,
      TAFD = as.numeric(difftime(DTC, RFSTDTC, units = "hours"))
    ) %>%
    nif::new_nif()
}


# Helper function to create mock sdtm object
create_mock_sdtm <- function() {
  # Create DM domain with required columns
  dm <- tribble(
    ~USUBJID, ~RFSTDTC, ~ACTARMCD, ~SEX, ~AGE, ~RACE,
       "001", "2023-01-01 08:00:00", "TRT", "F", 45, "WHITE",
       "002", "2023-01-01 08:00:00", "TRT", "M", 52, "WHITE"
  ) %>%
    mutate(
      RFSTDTC = as.POSIXct(RFSTDTC),
      DOMAIN = "DM"
    )

  # Create VS domain
  vs <- tribble(
    ~USUBJID, ~VSTESTCD, ~VSORRES, ~VSSTRESN, ~VSDTC,
       "001", "SYSBP", "120", 120, "2023-01-01 08:00:00",
       "001", "SYSBP", "125", 125, "2023-01-01 10:00:00",
       "002", "SYSBP", "118", 118, "2023-01-01 08:00:00",
       "002", "SYSBP", "130", 130, "2023-01-01 10:00:00"
  ) %>%
    mutate(
      VSDTC = as.POSIXct(VSDTC),
      DOMAIN = "VS"
    )

  # Create AE domain for events (fix: use AEDTC, not AESTDTC)
  ae <- tribble(
    ~USUBJID, ~AESEQ, ~AETESTCD, ~AEDTC, ~AESEV, ~AESER,
       "001", 1, "HEADACHE", "2023-01-01 09:00:00", "MILD", "N",
       "001", 2, "NAUSEA", "2023-01-01 11:00:00", "MODERATE", "Y",
       "002", 1, "FATIGUE", "2023-01-01 09:00:00", "MILD", "N"
  ) %>%
    mutate(
      AEDTC = as.POSIXct(AEDTC),
      DOMAIN = "AE"
    )

  # Create sdtm object structure
  sdtm <- list(
    domains = list(
      dm = dm,
      vs = vs,
      ae = ae
    )
  )

  class(sdtm) <- "sdtm"
  return(sdtm)
}


test_that("add_event_observation validates input parameters correctly", {
  # Create mock data
  mock_nif <- create_mock_nif()
  mock_sdtm <- create_mock_sdtm()

  # Test error for non-nif object
  expect_error(
    add_event_observation(
      nif = data.frame(),
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'"
    ),
    "nif must be an nif object"
  )

  # Test error for non-sdtm object
  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = list(),
      domain = "ae",
      event_filter = "AESEV == 'MILD'"
    ),
    "sdtm must be an sdtm object"
  )

  # Test error for invalid metabolite parameter
  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      testcd = "HEADACHE",
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      metabolite = "TRUE"
    ),
    "metabolite must be a single logical value"
  )

  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      domain = "ae",
      testcd = "HEADACHE",
      event_filter = "AESEV == 'MILD'",
      metabolite = c(TRUE, FALSE)
    ),
    "metabolite must be a single logical value"
  )
})


test_that("add_event_observation handles analyte and parent parameters correctly", {
  mock_nif <- create_mock_nif()
  mock_sdtm <- create_mock_sdtm()

  # Test error when both analyte and testcd are NULL
  expect_error(
    mock_nif %>%
      add_event_observation(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      analyte = NULL,
      testcd = NULL,
      cmt = 2,
      silent = TRUE
    ),
    "analyte and testcd cannot be both NULL!"
  )
})


test_that("add_event_observation handles compartment assignment correctly", {
  mock_nif <- create_mock_nif()
  mock_sdtm <- create_mock_sdtm()

  # Test warning for duplicate compartment
  expect_warning({
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      domain = "ae",
      testcd = "HEADACHE",
      event_filter = "AESEV == 'MILD'",
      cmt = 1,  # Already exists in mock_nif
      silent = TRUE
    )
  }, "Compartment 1 is already assigned!")
})


test_that("add_event_observation handles debug parameter correctly", {
  mock_nif <- create_mock_nif()
  mock_sdtm <- create_mock_sdtm()

  # Test that debug parameter is accepted without error
  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      domain = "ae",
      testcd = "HEADACHE",
      event_filter = "AESEV == 'MILD'",
      debug = "not_logical",
      silent = TRUE
    ),
    "debug must be a single logical value"
  )
})


test_that("add_event_observation handles event_diff parameter correctly", {
  mock_nif <- create_mock_nif()
  mock_sdtm <- create_mock_sdtm()

  # Test error for invalid event_diff parameter
  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      domain = "ae",
      testcd = "HEADACHE",
      event_filter = "AESEV == 'MILD'",
      event_diff = "not_logical",
      silent = TRUE
    ),
    "event_diff must be a single logical value"
  )
})


test_that("add_event_observation handles keep parameter correctly", {
  mock_nif <- create_mock_nif()
  mock_sdtm <- create_mock_sdtm()

  # Test that keep parameter accepts character vectors
  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      domain = "ae",
      testcd = "HEADACHE",
      event_filter = "AESEV == 'MILD'",
      keep = 123,  # Should be character
      silent = TRUE
    ),
    "keep must be a single character string"
  )
})


test_that("add_event_observation returns correct object structure", {
  mock_nif <- create_mock_nif()
  mock_sdtm <- create_mock_sdtm()

  # Test that function returns a nif object when successful
  # Note: This test may fail if nif package dependencies are not available
  # In that case, we focus on input validation tests
  skip_if_not_installed("nif")

  result <- add_event_observation(
    nif = mock_nif,
    sdtm = mock_sdtm,
    domain = "ae",
    testcd = "HEADACHE",
    event_filter = "AESEV == 'MILD'",
    silent = TRUE
  )

  # Check that result is a nif object
  expect_true(inherits(result, "nif"))
  expect_true(inherits(result, "data.frame"))
})


test_that("add_event_observation handles edge cases", {
  mock_nif <- create_mock_nif()
  mock_sdtm <- create_mock_sdtm()

  # Test with empty nif object (should error)
  empty_nif <- mock_nif[0, ]
  class(empty_nif) <- c("nif", "data.frame")

  expect_warning(
    expect_error(
    add_event_observation(
      nif = empty_nif,
      sdtm = mock_sdtm,
      domain = "ae",
      testcd = "HEADACHE",
      event_filter = "AESEV == 'MILD'",
      silent = TRUE
    ),
    "Please add at least one administration first!"
  ))

  # Test with non-existent domain
  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      domain = "nonexistent",
      testcd = "HEADACHE",
      event_filter = "AESEV == 'MILD'",
      silent = TRUE
    ),
    "Domain 'nonexistent' not found in sdtm object"
  )
})


test_that("add_event_observation validates character parameters", {
  mock_nif <- create_mock_nif()
  mock_sdtm <- create_mock_sdtm()

  # Test domain parameter validation
  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      domain = 123,  # Should be character
      testcd = "HEADACHE",
      event_filter = "AESEV == 'MILD'",
      silent = TRUE
    ),
    "domain must be a single character string"
  )

  # Test event_filter parameter validation
  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      domain = "ae",
      testcd = "HEADACHE",
      event_filter = 123,  # Should be character
      silent = TRUE
    ),
    "event_filter must be a single character string"
  )

  # Test analyte parameter validation
  expect_error(
    add_event_observation(
      nif = mock_nif,
      sdtm = mock_sdtm,
      domain = "ae",
      testcd = "HEADACHE",
      event_filter = "AESEV == 'MILD'",
      analyte = 123,  # Should be character
      silent = TRUE
    ),
    "analyte must be a single character string"
  )
})
