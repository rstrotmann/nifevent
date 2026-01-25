test_that("comples pharmaverse example works as intended", {
  sdtm <- nif::cdiscpilot01_sdtm

  ae_lut <- tibble::tribble(
      ~AESEV, ~DV,
      "MILD",   1,
  "MODERATE",   2,
    "SEVERE",   3
  )

  expect_no_error(
    nif <- nif(sdtm, XAN ~ XANOMELINE, silent = TRUE) |>
      add_observation(sdtm, "lb", "HGB", silent = TRUE) |>
      add_event_observation(
        sdtm, "lb", testcd = "HGB", analyte = "ANEMIA",
        "LBSTRESN < 8.5", silent = TRUE) |>
      derive_cfb()
  )

  temp <- make_surv_dataset(nif, "ANEMIA", silent = TRUE)
  expect_equal(nrow(temp), 231)
})
