# Kaplan-Meier plot for events

**\[experimental\]**

## Usage

``` r
kmplot(
  nif,
  analyte,
  dose = NULL,
  group = NULL,
  convert_tafd_h_to_d = TRUE,
  title = NULL,
  y_label = NULL,
  show_censoring = TRUE,
  show_risk_table = TRUE,
  show_ci = TRUE,
  silent = NULL
)
```

## Arguments

- nif:

  A nif object.

- analyte:

  The analyte.

- dose:

  The dose, defaults to all doses, if NULL.

- group:

  Grouping variable.

- convert_tafd_h_to_d:

  Convert the TAFD field from hours to days, defaults to TRUE.

- title:

  The plot title, defaults to none if NULL.

- y_label:

  The y axis label, defaults to the analyte, if NULL.

- show_censoring:

  Add censor mark, as logical.

- show_risk_table:

  Add risk table, as logical.

- show_ci:

  Add confidence interval, as logical.

- silent:

  Suppress messages, as logical. Defaults to nif_option setting if NULL.

## Value

A ggsurvfit (ggplot) object.
