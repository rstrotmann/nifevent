#' Kaplan-Meier plot for events
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param nif A nif object.
#' @param analyte The analyte.
#' @param dose The dose, defaults to all doses, if NULL.
#' @param group Grouping variable.
#'
#' @returns A ggplot object.
#'
#' @importFrom survival survfit
#' @importFrom survminer ggsurvplot
#' @export
kmplot <- function(
    nif,
    analyte,
    dose = NULL,
    group = NULL
  ) {
  # Validate input is a NIF object
  if (!inherits(nif, "nif")) {
    stop("Input must be a NIF object")
  }

  if (is.null(dose)) {
    dose <- unique(filter(nif, .data$EVID == 0)$DOSE)
  }

  if(is.null(group)) {
    group <- 1
  }

  temp <- nif %>%
    as.data.frame() %>%
    filter(
      .data$EVID == 0,
      .data$ANALYTE == analyte,
      .data$DOSE %in% dose
    ) %>%
    mutate(TIMED = .data$TAFD / 24)

  if(!is.null(group) & all(group %in% names(temp))) {
    temp <- temp %>%
      tidyr::unite(group, all_of(group))
  } else {
    temp <- temp %>%
      mutate(group = 1)
  }

  sf <- survival::survfit(Surv(TIMED, DV) ~ group, data = temp)
  survminer::ggsurvplot(sf)
}
