#' Prepare data set for survival analysis
#'
#' @details
#' The time variable is TAFD. All events with TAFD < 0 are deleted.
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
#' @keywords internal
#' @noRd
make_surv_dataset <- function(
    nif,
    analyte,
    group = NULL,
    convert_tafd_h_to_d = TRUE,
    silent = NULL) {
  # Validate analyte parameter
  if (is.null(analyte) || !is.character(analyte) || length(analyte) != 1) {
    stop("analyte must be a single character string")
  }

  # Check if analyte exists in the data
  if (!analyte %in% unique(nif$ANALYTE)) {
    stop(paste0("No data found for analyte '", analyte, "'"))
  }

  # Filter data for the analyte
  analyte_data <- nif %>%
    as.data.frame() %>%
    # filter(.data$TAFD >= 0) %>%
    filter(.data$ANALYTE == analyte) %>%
    filter(.data$EVID == 0)

  # Filter data for positive TAFD
  neg_tafd <- filter(analyte_data, .data$TAFD < 0)
  if(nrow(neg_tafd) > 0) {
    nif:::conditional_message(
      nrow(neg_tafd) ,
      plural(" row", nrow(neg_tafd) > 1),
      " with negative TAFD removed from survival data set!",
      silent = silent
    )
    analyte_data <- analyte_data %>%
      filter(.data$TAFD >= 0)
  }

  # Check if we have any data after filtering
  if (nrow(analyte_data) == 0) {
    stop(paste0("No event data found for analyte '", analyte, "'"))
  }

  # Check for negative or missing TAFD values
  # if (any(analyte_data$TAFD < 0, na.rm = TRUE)) {
  #   stop("Negative time values found for events")
  # }

  if (any(is.na(analyte_data$TAFD))) {
    stop("Missing time values found for events")
  }

  result <- analyte_data %>%
    {if(convert_tafd_h_to_d == TRUE)
      mutate(., TIMED = .data$TAFD/24) else .} %>%
    group_by(.data$ID) %>%
    mutate(ev_first = min(c(.data$TIMED[.data$DV == 1], Inf))) %>%
    mutate(ev_lastobs = max(.data$TIMED)) %>%
    ungroup() %>%
    select("ID", any_of(c(group)), "ev_first", "ev_lastobs") %>%
    distinct() %>%
    rowwise() %>%
    mutate(time = min(c(.data$ev_first, .data$ev_lastobs))) %>%
    mutate(status = case_when(
      is.infinite(.data$ev_first) ~ 0, .default = 1))

  # Check if we have any events
  if (all(result$status == 0)) {
    warning(paste0("No events found for analyte '", analyte, "'"))
  }

  return(result)
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
#' @param ... Further arguments to ggsurvplot.
#' @param group Grouping variable.
#' @param silent Suppress messages, as logical. Defaults to nif_option setting
#'   if NULL.
#' @param convert_tafd_h_to_d Convert the TAFD field from hours to days,
#'   defaults to TRUE.
#'
#' @inheritDotParams survminer::ggsurvplot risk.table pval conf.int surv.median.line
#'
#' @returns A ggplot object.
#'
#' @importFrom survival survfit
#' @importFrom survminer ggsurvplot
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
    silent = NULL,
    ...
  ) {
  # Validate input is a NIF object
  if (!inherits(nif, "nif")) {
    stop("Input must be a NIF object")
  }

  # Validate analyte parameter
  if (is.null(analyte) || !is.character(analyte) || length(analyte) != 1) {
    stop("analyte must be a single character string")
  }

  # Check if analyte exists in the data
  if (!analyte %in% unique(nif$ANALYTE)) {
    stop(paste0("No data found for analyte '", analyte, "'"))
  }

  # Handle dose filtering
  if (is.null(dose)) {
    dose <- unique(filter(nif, .data$EVID == 0)$DOSE)
  } else {
    # Validate that specified dose exists
    available_doses <- unique(filter(nif, .data$EVID == 0)$DOSE)
    if (!all(dose %in% available_doses)) {
      missing_doses <- setdiff(dose, available_doses)
      stop(paste0(
        "No data found for specified ",
        nif::plural("dose", length(missing_doses) > 1),
        ": ",
        nif::nice_enumeration(missing_doses)
      ))
    }
  }

  # Filter data by dose
  filtered_nif <- filter(nif, .data$DOSE %in% dose)

  # Validate group variable if provided
  if (!is.null(group)) {
    # if (!is.character(group) || length(group) != 1) {
    if (!is.character(group)) {
      stop("group must be a character string")
    }
    if (!group %in% names(filtered_nif)) {
      stop(paste0("Group variable '", group, "' not found in data"))
    }
  }

  # Create survival dataset
  temp <- make_surv_dataset(
    nif = filtered_nif,
    analyte = analyte,
    group = group,
    convert_tafd_h_to_d = convert_tafd_h_to_d,
    silent = silent)

  # Check if we have any data for analysis
  if (nrow(temp) == 0) {
    stop("No data available for analysis")
  }

  # Handle grouping
  if(!is.null(group) & all(group %in% names(temp))) {
    temp <- temp %>%
      tidyr::unite(group, all_of(group))
  } else {
    temp <- temp %>%
      mutate(group = 1)
  }

  # Create survival fit
  sf <- survival::survfit(Surv(time, status) ~ group, data = temp)

  # Store the data in the survival fit object for survminer
  sf$data <- temp

  if(!is.null(sf$strata)) {
    names(sf$strata) <- gsub("group=", "", names(sf$strata))
  }

  # Create plot
  p <- survminer::ggsurvplot(sf, data = temp, ...)

  # Handle legend and labels
  legend <- if (!is.null(group)) nif::nice_enumeration(group) else NULL
  if(is.null(y_label)) {
    y_label <- paste0("S[", analyte, "]")
  }

  p$plot <- p$plot +
    ggplot2::labs(fill = legend, color = legend, y = y_label,
                  title = title)

  if(is.null(sf$strata)) {
    p$plot <- p$plot +
      theme(legend.position = "none")
  }

  return(p)
}
