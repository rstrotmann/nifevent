# Helper function to create mock sdtm object
create_mock_sdtm <- function() {
  # Create DM domain with required columns
  dm <- tibble::tribble(
    ~USUBJID,              ~RFSTDTC, ~ACTARMCD, ~SEX, ~AGE,    ~RACE,
       "001", "2023-01-01 08:00:00",     "TRT",  "F",   45,  "WHITE",
       "002", "2023-01-01 08:00:00",     "TRT",  "M",   52,  "WHITE",
       "003", "2023-01-01 08:00:00", "SCRNFAIL",  "M",   30,  "ASIAN"
  ) %>%
    dplyr::mutate(
      RFSTDTC = as.POSIXct(RFSTDTC),
      DOMAIN = "DM"
    )

  # Create VS domain
  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSORRES, ~VSSTRESN,              ~VSDTC,
       "001",   "SYSBP",    "120",       120, "2023-01-01 08:00:00",
       "001",   "SYSBP",    "125",       125, "2023-01-01 10:00:00",
       "002",   "SYSBP",    "118",       118, "2023-01-01 08:00:00",
       "002",   "SYSBP",    "130",       130, "2023-01-01 10:00:00"
  ) %>%
    dplyr::mutate(
      VSDTC = as.POSIXct(VSDTC),
      DOMAIN = "VS"
    )

  # Create AE domain for events with multiple observations per subject
  ae <- tibble::tribble(
    ~USUBJID, ~AESEQ, ~AETESTCD,              ~AEDTC,    ~AESEV, ~AESER,              ~AESTDTC,              ~AEENDTC,
       "001",      1, "HEADACHE", "2023-01-01 09:00:00",  "MILD",    "N", "2023-01-01 09:00:00", "2023-01-01 10:00:00",
       "001",      2,   "NAUSEA", "2023-01-01 11:00:00", "MODERATE", "Y", "2023-01-01 11:00:00", "2023-01-01 12:00:00",
       "001",      3, "HEADACHE", "2023-01-01 13:00:00", "SEVERE",   "N", "2023-01-01 13:00:00", "2023-01-01 14:00:00",
       "002",      1,  "FATIGUE", "2023-01-01 09:00:00",  "MILD",    "N", "2023-01-01 09:00:00", "2023-01-01 10:00:00",
       "002",      2, "HEADACHE", "2023-01-01 11:00:00",  "MILD",    "N", "2023-01-01 11:00:00", "2023-01-01 12:00:00",
       "003",      1, "HEADACHE", "2023-01-01 09:00:00",  "MILD",    "N", "2023-01-01 09:00:00", "2023-01-01 10:00:00"
  ) %>%
    dplyr::mutate(
      AEDTC = as.POSIXct(AEDTC),
      AESTDTC = as.POSIXct(AESTDTC),
      AEENDTC = as.POSIXct(AEENDTC),
      DOMAIN = "AE"
    )

  # Create CM domain (concomitant medications) for testing different domain
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ,  ~CMDECOD,              ~CMSTDTC,              ~CMENDTC,
       "001",      1,  "ASPIRIN", "2023-01-01 08:00:00", "2023-01-05 08:00:00",
       "001",      2, "IBUPROFEN", "2023-01-02 08:00:00", "2023-01-06 08:00:00",
       "002",      1,  "ASPIRIN", "2023-01-01 08:00:00", "2023-01-05 08:00:00"
  ) %>%
    dplyr::mutate(
      CMSTDTC = as.POSIXct(CMSTDTC),
      CMENDTC = as.POSIXct(CMENDTC),
      DOMAIN = "CM"
    )

  # Create sdtm object structure
  sdtm <- list(
    domains = list(
      dm = dm,
      vs = vs,
      ae = ae,
      cm = cm
    )
  )

  class(sdtm) <- "sdtm"
  return(sdtm)
}


test_that("make_event validates sdtm input", {
  mock_sdtm <- create_mock_sdtm()

  # Test error for non-sdtm object
  expect_error(
    make_event(
      sdtm = list(),
      domain = "ae",
      event_filter = "AESEV == 'MILD'"
    ),
    "sdtm must be an sdtm object"
  )

  # Test error for NULL sdtm
  expect_error(
    make_event(
      sdtm = NULL,
      domain = "ae",
      event_filter = "AESEV == 'MILD'"
    ),
    "sdtm must be an sdtm object"
  )

  # Test success with valid sdtm
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      silent = TRUE
    ),
    NA
  )
})


test_that("make_event validates domain parameter", {
  mock_sdtm <- create_mock_sdtm()

  # Test error for non-character domain
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = 123,
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE"
    ),
    "domain must be a single character string"
  )

  # Test error for NULL domain
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = NULL,
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE"
    ),
    "domain must be a single character string"
  )

  # Test error for empty domain
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE"
    ),
    "domain must be a non-empty character string"
  )

  # Test error for non-existent domain
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "nonexistent",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE"
    ),
    "Domain 'nonexistent' not found in sdtm object"
  )

  # Test success with valid domain (case insensitive)
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "AE",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      silent = TRUE
    ),
    NA
  )

  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      silent = TRUE
    ),
    NA
  )
})


test_that("make_event validates analyte and testcd parameters", {
  mock_sdtm <- create_mock_sdtm()

  # Test error when both analyte and testcd are NULL
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      analyte = NULL,
      testcd = NULL
    ),
    "analyte and testcd cannot be both NULL!"
  )

  # Test success with testcd only (analyte should be auto-generated)
  result <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )
  expect_true("EV_HEADACHE" %in% result$ANALYTE)

  # Test success with analyte only
  result2 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    analyte = "CUSTOM_ANALYTE",
    silent = TRUE
  )
  expect_true("CUSTOM_ANALYTE" %in% result2$ANALYTE)

  # Test success with both analyte and testcd (analyte should take precedence)
  result3 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    analyte = "CUSTOM_ANALYTE",
    silent = TRUE
  )
  expect_true("CUSTOM_ANALYTE" %in% result3$ANALYTE)
  expect_false("EV_HEADACHE" %in% result3$ANALYTE)

  # Test error for non-character analyte
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      analyte = 123
    ),
    "analyte must be a single character string"
  )

  # Test error for non-character testcd
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = 123
    ),
    "testcd must be a single character string"
  )
})


test_that("make_event validates event_filter parameter", {
  mock_sdtm <- create_mock_sdtm()

  # Test error for non-character event_filter
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = 123,
      testcd = "HEADACHE"
    ),
    "event_filter must be a single character string"
  )

  # Test error for NULL event_filter
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = NULL,
      testcd = "HEADACHE"
    ),
    "event_filter must be a single character string"
  )

  # Test error for invalid filter expression
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "INVALID_COLUMN == 'VALUE'",
      testcd = "HEADACHE",
      silent = TRUE
    ),
    "event filter.*is not valid"
  )

  # Test success with valid filter
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      silent = TRUE
    ),
    NA
  )
})


test_that("make_event validates observation_filter parameter", {
  mock_sdtm <- create_mock_sdtm()

  # Test error for non-character observation_filter
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      observation_filter = 123
    ),
    "observation_filter must be a single character string"
  )

  # Test error for invalid observation_filter
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      observation_filter = "INVALID_COLUMN == 'VALUE'",
      silent = TRUE
    ),
    "observation filter.*is not valid"
  )

  # Test error when observation_filter returns no entries
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      observation_filter = "AESEV == 'NONEXISTENT'",
      silent = TRUE
    ),
    "The observation_filter.*returned no entries"
  )

  # Test success with valid observation_filter
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      observation_filter = "AESER == 'N'",
      silent = TRUE
    ),
    NA
  )
})


test_that("make_event validates testcd parameter", {
  mock_sdtm <- create_mock_sdtm()

  # Test error when testcd not found after observation_filter
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "NONEXISTENT",
      silent = TRUE
    ),
    "testcd.*not found after filtering for observation_filter"
  )

  # Test success with valid testcd
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      silent = TRUE
    ),
    NA
  )

  # Test success with NULL testcd when analyte is provided
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      analyte = "CUSTOM_ANALYTE",
      silent = TRUE
    ),
    NA
  )
})


test_that("make_event validates logical parameters", {
  mock_sdtm <- create_mock_sdtm()

  # Test error for invalid event_diff
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      event_diff = "not_logical"
    ),
    "event_diff must be a single logical value"
  )

  # Test error for invalid metabolite
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      metabolite = "not_logical"
    ),
    "metabolite must be a single logical value"
  )

  # Test error for invalid silent
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      silent = "not_logical"
    ),
    "silent must be a single logical value"
  )

  # Test success with valid logical parameters
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      event_diff = TRUE,
      metabolite = TRUE,
      silent = TRUE
    ),
    NA
  )
})


test_that("make_event validates numeric parameters", {
  mock_sdtm <- create_mock_sdtm()

  # Test error for invalid cmt
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      cmt = "not_numeric"
    ),
    "cmt must be a numeric value"
  )

  # Test success with valid cmt
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      cmt = 2,
      silent = TRUE
    ),
    NA
  )

  # Test success with NULL cmt
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      cmt = NULL,
      silent = TRUE
    ),
    NA
  )
})


test_that("make_event validates character parameters", {
  mock_sdtm <- create_mock_sdtm()

  # Test error for invalid parent
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      parent = 123
    ),
    "parent must be a single character string"
  )

  # Test error for invalid subject_filter
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      subject_filter = 123
    ),
    "subject_filter must be a single character string"
  )

  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      dtc_field = 123
    ),
    "dtc_field must be a single character string"
  )

  # Test error for invalid keep (should allow multiple)
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      keep = 123
    ),
    "keep must be a single character string"
  )

  # Test success with valid keep (multiple values)
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      keep = c("AESEV", "AESER"),
      silent = TRUE
    ),
    NA
  )
})


test_that("make_event applies subject_filter correctly", {
  mock_sdtm <- create_mock_sdtm()

  # Test default subject_filter excludes screen failures
  result_default <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )
  expect_false("003" %in% result_default$USUBJID)

  # Test custom subject_filter
  result_custom <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    subject_filter = "ACTARMCD == 'TRT'",
    silent = TRUE
  )
  expect_false("003" %in% result_custom$USUBJID)

  # Test subject_filter that includes all subjects
  result_all <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    subject_filter = "TRUE",
    silent = TRUE
  )
  expect_true("003" %in% result_all$USUBJID)
})


test_that("make_event flags events correctly", {
  mock_sdtm <- create_mock_sdtm()

  # Test event_filter flags correct events
  result <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )

  # All returned rows should have DV = 1 (event occurred)
  expect_true(all(result$DV == 1))

  # Test different event_filter
  result_severe <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'SEVERE'",
    testcd = "HEADACHE",
    silent = TRUE
  )
  expect_true(all(result_severe$DV == 1))
  expect_true(nrow(result_severe) > 0)

  # Test event_filter with multiple conditions
  result_serious <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESER == 'Y'",
    testcd = "NAUSEA",
    silent = TRUE
  )
  expect_true(all(result_serious$DV == 1))
  expect_equal(nrow(result_serious), 1)
})


test_that("make_event applies event_diff correctly", {
  mock_sdtm <- create_mock_sdtm()

  # Create data with multiple events where condition changes
  # Subject 001 has: MILD (flag=1), then MODERATE (flag=0), then SEVERE (flag=0)
  # With event_diff=FALSE, should get all MILD events
  result_no_diff <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    event_diff = FALSE,
    silent = TRUE
  )
  # Should include all MILD events
  expect_true(nrow(result_no_diff) >= 2)

  # With event_diff=TRUE, should only get transitions from FALSE to TRUE
  # This is more complex and depends on the ordering of observations
  result_diff <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    event_diff = TRUE,
    silent = TRUE
  )
  # Should have fewer or equal rows compared to no_diff
  expect_true(nrow(result_diff) <= nrow(result_no_diff))
  expect_true(all(result_diff$DV == 1))
})


test_that("make_event returns correct output structure", {
  mock_sdtm <- create_mock_sdtm()

  result <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )

  # Check that result is a data frame
  expect_true(is.data.frame(result))

  # Check required columns
  required_cols <- c(
    "USUBJID", "ANALYTE", "DV", "TIME", "CMT", "AMT", "DOSE",
    "PARENT", "METABOLITE", "EVID", "MDV", "IMPUTATION", "TRTDY", "DTC"
  )
  expect_true(all(required_cols %in% names(result)))

  # Check that DV is always 1 for events
  expect_true(all(result$DV == 1))

  # Check that EVID is 0 for observations
  expect_true(all(result$EVID == 0))

  # Check that AMT is 0 for observations
  expect_true(all(result$AMT == 0))

  # Check that MDV is numeric
  expect_true(is.numeric(result$MDV))

  # Check that ANALYTE is set correctly
  expect_true(all(result$ANALYTE == "EV_HEADACHE"))

  # Check that PARENT defaults to ANALYTE
  expect_true(all(result$PARENT == result$ANALYTE))
})


test_that("make_event sets analyte and parent correctly", {
  mock_sdtm <- create_mock_sdtm()

  # Test with testcd only (analyte auto-generated)
  result1 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )
  expect_true(all(result1$ANALYTE == "EV_HEADACHE"))
  expect_true(all(result1$PARENT == "EV_HEADACHE"))

  # Test with custom analyte
  result2 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    analyte = "CUSTOM_ANALYTE",
    silent = TRUE
  )
  expect_true(all(result2$ANALYTE == "CUSTOM_ANALYTE"))
  expect_true(all(result2$PARENT == "CUSTOM_ANALYTE"))

  # Test with custom parent
  result3 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    analyte = "CUSTOM_ANALYTE",
    parent = "PARENT_ANALYTE",
    silent = TRUE
  )
  expect_true(all(result3$ANALYTE == "CUSTOM_ANALYTE"))
  expect_true(all(result3$PARENT == "PARENT_ANALYTE"))
})


test_that("make_event sets metabolite and cmt correctly", {
  mock_sdtm <- create_mock_sdtm()

  # Test default metabolite = FALSE
  result1 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )
  expect_true(all(result1$METABOLITE == FALSE))

  # Test metabolite = TRUE
  result2 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    metabolite = TRUE,
    silent = TRUE
  )
  expect_true(all(result2$METABOLITE == TRUE))

  # Test cmt = NULL (should be NULL in output)
  result3 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    cmt = NULL,
    silent = TRUE
  )
  expect_true(all(is.na(result3$CMT)))

  # Test cmt = 2
  result4 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    cmt = 2,
    silent = TRUE
  )
  expect_true(all(result4$CMT == 2))
})


test_that("make_event calculates TRTDY correctly", {
  mock_sdtm <- create_mock_sdtm()

  result <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )

  # TRTDY should be calculated as days from RFSTDTC
  # RFSTDTC is 2023-01-01 08:00:00
  # First event is 2023-01-01 09:00:00, so TRTDY should be 1
  expect_true(all(result$TRTDY >= 1))
  expect_true(is.numeric(result$TRTDY))

  # Check that TRTDY is calculated per subject
  subject_001 <- result %>% dplyr::filter(USUBJID == "001")
  if (nrow(subject_001) > 0) {
    # All events for same subject on same day should have TRTDY = 1
    same_day_events <- subject_001 %>% dplyr::filter(lubridate::date(DTC) == lubridate::date("2023-01-01"))
    if (nrow(same_day_events) > 0) {
      expect_true(all(same_day_events$TRTDY == 1))
    }
  }
})


test_that("make_event handles dtc_field parameter", {
  mock_sdtm <- create_mock_sdtm()

  # Test default dtc_field (should be AEDTC for AE domain)
  result1 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )
  expect_true("DTC" %in% names(result1))
  expect_false(any(is.na(result1$DTC)))

  # Test custom dtc_field
  result2 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    dtc_field = "AESTDTC",
    silent = TRUE
  )
  expect_true("DTC" %in% names(result2))
  expect_false(any(is.na(result2$DTC)))
})


test_that("make_event handles keep parameter", {
  mock_sdtm <- create_mock_sdtm()

  # Test without keep parameter
  result1 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )

  # Test with keep parameter
  result2 <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    keep = c("AESEV", "AESER"),
    silent = TRUE
  )

  # Check that kept columns are present
  expect_true("AESEV" %in% names(result2))
  expect_true("AESER" %in% names(result2))
})


test_that("make_event handles different domains", {
  mock_sdtm <- create_mock_sdtm()

  # Test with CM domain
  result_cm <- make_event(
    sdtm = mock_sdtm,
    domain = "cm",
    event_filter = "CMDECOD == 'ASPIRIN'",
    analyte = "EV_ASPIRIN",
    dtc_field = "CMSTDTC",
    silent = TRUE
  )

  expect_true(is.data.frame(result_cm))
  expect_true(nrow(result_cm) > 0)
  expect_true(all(result_cm$ANALYTE == "EV_ASPIRIN"))
})


test_that("make_event handles edge cases", {
  mock_sdtm <- create_mock_sdtm()

  # Test with event_filter that matches no events
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'NONEXISTENT'",
      testcd = "HEADACHE",
      silent = TRUE
    ),
    NA  # Should return empty data frame or handle gracefully
  )

  # Test with observation_filter that filters out all testcd matches
  expect_error(
    make_event(
      sdtm = mock_sdtm,
      domain = "ae",
      event_filter = "AESEV == 'MILD'",
      testcd = "HEADACHE",
      observation_filter = "AETESTCD == 'NAUSEA'",
      silent = TRUE
    ),
    "testcd.*not found after filtering"
  )

  # Test with missing DTC values (should filter them out)
  # This is handled by the filter(!is.na(.data$DTC)) in the function
  result <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )
  expect_false(any(is.na(result$DTC)))
})


test_that("make_event handles SRC_DOMAIN and SRC_SEQ", {
  mock_sdtm <- create_mock_sdtm()

  result <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    keep = c("SRC_DOMAIN", "SRC_SEQ"),
    silent = TRUE
  )

  # SRC_DOMAIN should be set
  expect_true("SRC_DOMAIN" %in% names(result))
  expect_true(all(result$SRC_DOMAIN == "AE"))

  # SRC_SEQ should be set if AESEQ exists
  expect_true("SRC_SEQ" %in% names(result))
  expect_false(any(is.na(result$SRC_SEQ)))
})


test_that("make_event handles TIME and DOSE fields", {
  mock_sdtm <- create_mock_sdtm()

  result <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )

  # TIME should be NA for events
  expect_true(all(is.na(result$TIME)))

  # DOSE should be NA for events
  expect_true(all(is.na(result$DOSE)))
})


test_that("make_event handles IMPUTATION field", {
  mock_sdtm <- create_mock_sdtm()

  result <- make_event(
    sdtm = mock_sdtm,
    domain = "ae",
    event_filter = "AESEV == 'MILD'",
    testcd = "HEADACHE",
    silent = TRUE
  )

  # IMPUTATION should be empty string
  expect_true("IMPUTATION" %in% names(result))
  expect_true(all(result$IMPUTATION == ""))
})

