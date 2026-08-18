# References:
# https://bioconnector.github.io/workshops/r-survival.html


#' Prepare data set for survival analysis
#'
#' @details
#' The time variable is TAFD. All events with TAFD < 0 are deleted.
#'
#' For the 'analyte' analyte, the data set must contain a DV that only contains
#' 0 and 1 or FALSE and TRUE. DV must not contain NA values.
#'
#'
#' @param nif A nif data set.
#' @param analyte The analyte as character.
#' @param group The grouping variable, as character.
#' @param silent Suppress messages, as logical. Defaults to nif_option setting
#'   if NULL.
#' @param convert_tafd_h_to_d Convert the TAFD field from hours to days,
#'   defaults to TRUE.
#' @returns A data frame.
#' @import dplyr
#' @importFrom nif plural
#' @importFrom nif nice_enumeration
#' @keywords internal
#' @noRd
make_surv_dataset <- function(
  nif,
  analyte,
  group = NULL,
  convert_tafd_h_to_d = TRUE,
  silent = NULL
) {
  # INPUT VALIDATIONS
  nif:::validate_argument(analyte, "character")
  nif:::validate_argument(group, "character", allow_multiple = TRUE, allow_null = TRUE)
  nif:::validate_nif(nif, group)
  nif:::validate_analyte(nif, analyte)
  nif:::validate_argument(convert_tafd_h_to_d, "logical")
  nif:::validate_argument(silent, "logical", allow_null = TRUE)

  # DATA PREPROCESSING AND VALIDATION
  # Filter data for the analyte
  analyte_data <- nif |>
    as.data.frame() |>
    filter(.data$ANALYTE == analyte) |>
    filter(.data$EVID == 0)

  # Make sure DV has only 0 or 1, or FALSE or TRUE, but no NA values
  if (any(is.na(analyte_data$DV))) {
    stop("DV for the analyte cannot contain NA values")
  }

  if (!all(unique(analyte_data$DV) %in% c(0, 1, FALSE, TRUE))) {
    stop("DV for the analyte must be either 0 or 1, or FALSE or TRUE")
  }

  analyte_data <- analyte_data |>
    mutate(DV = as.numeric(.data$DV))

  # Filter data for positive TAFD
  neg_tafd <- filter(analyte_data, .data$TAFD < 0)
  if (nrow(neg_tafd) > 0) {
    nif:::conditional_message(
      nrow(neg_tafd),
      plural(" row", nrow(neg_tafd) > 1),
      " with negative TAFD removed from survival data set!",
      silent = silent
    )
    analyte_data <- analyte_data |>
      filter(.data$TAFD >= 0)
  }

  # Check if we have any data after filtering
  if (nrow(analyte_data) == 0) {
    stop(paste0("No event data found for analyte '", analyte, "'"))
  }

  if (any(is.na(analyte_data$TAFD))) {
    stop("Missing time values found for events")
  }

  # SURVIVAL DATA SET CREATION
  events <- analyte_data |>
    filter(.data$DV == 1) |>
    mutate(status = 1, time = .data$TAFD) |>
    select(c("ID", "time", "status", any_of(group)))

  if (nrow(events) == 0) {
    nif:::conditional_message(
      "No events found for analyte '", analyte, "'",
      silent = silent
    )
  } else {
    events <- events |>
      group_by(.data$ID) |>
      filter(.data$time == min(.data$time, na.rm = TRUE)) |>
      ungroup() |>
      distinct()
  }

  ids <- unique(events$ID)

  censoring <- nif |>
    filter(.data$EVID == 0) |>
    reframe(time = max(.data$TAFD), .by = c("ID", any_of(group))) |>
    filter(!.data$ID %in% ids) |>
    mutate(status = 0)

  result <- rbind(events, censoring) |>
    arrange("ID", "time")

  if (convert_tafd_h_to_d == TRUE)
    result <- mutate(result, time = .data$time / 24)

  result
}


#' Kaplan-Meier plot for events
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param nif A nif object.
#' @param analyte The analyte.
#' @param dose The dose, defaults to all doses, if NULL.
#' @param title The plot title, defaults to none if NULL.
#' @param y_label The y axis label, defaults to the analyte, if NULL.
#' @param group Grouping variable.
#' @param silent Suppress messages, as logical. Defaults to nif_option setting
#'   if NULL.
#' @param convert_tafd_h_to_d Convert the TAFD field from hours to days,
#'   defaults to TRUE.
#' @param show_censoring Add censor mark, as logical.
#' @param show_risk_table Add risk table, as logical.
#' @param show_ci Add confidence interval, as logical.
#'
#' @returns A ggsurvfit (ggplot) object.
#'
#' @import ggsurvfit
#' @import ggplot2
#' @export
kmplot <- function(
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
) {
  # INPUT VALIDATIONS
  nif:::validate_argument(analyte, "character")
  nif:::validate_argument(group, "character", allow_multiple = TRUE, allow_null = TRUE)
  nif:::validate_nif(nif, group)
  nif:::validate_argument(dose, "numeric", allow_multiple = TRUE, allow_null = TRUE)
  nif:::validate_analyte(nif, analyte)
  nif:::validate_argument(convert_tafd_h_to_d, "logical")
  nif:::validate_argument(title, "character", allow_null = TRUE)
  nif:::validate_argument(y_label, "character", allow_null = TRUE)
  nif:::validate_argument(show_censoring, "logical")
  nif:::validate_argument(show_risk_table, "logical")
  nif:::validate_argument(show_ci, "logical")
  nif:::validate_argument(silent, "logical", allow_null = TRUE)

  # DATA PREPROCESSING
  # Dose filtering
  if (is.null(dose)) {
    dose <- unique(filter(nif, .data$EVID == 0)$DOSE)
  } else {
    # Validate that specified dose exists
    available_doses <- unique(filter(nif, .data$EVID == 0)$DOSE)
    if (!all(dose %in% available_doses)) {
      missing_doses <- setdiff(dose, available_doses)
      stop(paste0(
        "No data found for specified ",
        nif::plural("dose", length(missing_doses) > 1), ": ",
        nif::nice_enumeration(missing_doses)
      ))
    }
  }

  # Filter data by dose
  filtered_nif <- filter(nif, .data$DOSE %in% dose)

  # Create survival dataset
  temp <- make_surv_dataset(
    nif = filtered_nif,
    analyte = analyte,
    group = group,
    convert_tafd_h_to_d = convert_tafd_h_to_d,
    silent = silent
  ) |>
    filter(.data$time >= 0)

  # Check if we have any data for analysis
  if (nrow(temp) == 0) {
    stop("No data available for analysis")
  }

  # Handle grouping
  if (!is.null(group) && all(group %in% names(temp))) {
    temp <- temp |>
      tidyr::unite(group, all_of(group))
  } else {
    temp <- mutate(temp, group = 1)
  }

  p <- ggsurvfit::survfit2(Surv(time, status) ~ group, data = temp) |>
    ggsurvfit() +
    ylim(0, 1)

  if (show_ci == TRUE)
    p <- p + add_confidence_interval()

  if (show_risk_table == TRUE)
    p <- p + add_risktable()

  if (show_censoring == TRUE)
    p <- p + add_censor_mark()

  # Handle legend and labels
  legend <- if (!is.null(group)) nif::nice_enumeration(group) else NULL
  if (is.null(y_label)) {
    y_label <- paste0("S[", analyte, "]")
  }
  x_label <- ifelse(convert_tafd_h_to_d, "days", "hours")

  p +
    ggplot2::labs(
      fill = legend, color = legend,
      x = x_label, y = y_label,
      title = title
    )
}
