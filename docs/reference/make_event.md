# Make event observation

Make event observation

## Usage

``` r
make_event(
  sdtm,
  domain,
  event_filter = "TRUE",
  testcd = NULL,
  event_diff = FALSE,
  analyte = NULL,
  parent = NULL,
  metabolite = FALSE,
  cmt = NULL,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'SCREENFAIL', 'NOTTRT')",
  observation_filter = "TRUE",
  dtc_field = NULL,
  keep = NULL,
  silent = NULL
)
```

## Arguments

- sdtm:

  A sdtm object.

- domain:

  The domain code as character.

- event_filter:

  A filter term to characterize the events to extract, as character.

- testcd:

  The testcode from xxTESTCD where xx is the domain code, as character.
  Not used if NULL.

- event_diff:

  Only retain events where there is a change from the event filter
  evaluating to TRUE after being FALSE in the previous observation,
  i.e., apply time differentiation to the observation. As logical.
  Defaults to FALSE.

- analyte:

  The name for the analyte. Defaults to the 'EV_testcd', if NULL.

- parent:

  The name of the parent analyte for the observation as character.
  Defaults to the value of 'analyte' if NULL.

- metabolite:

  observation is a metabolite, as logical.

- cmt:

  The compartment for the observation, as numeric.

- subject_filter:

  The filter to be applied to the DM domain, as character.

- observation_filter:

  The filtering to apply to the observation source data, as character.

- dtc_field:

  The field to use as the date-time code for the observation. Defaults
  to 'xxDTC', with xx the domain name, if NULL.

- keep:

  Columns to keep, as character.

- silent:

  Suppress messages, as logical. Defaults to nif_option setting if NULL.

## Value

A data frame.
