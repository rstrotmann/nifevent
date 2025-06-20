#' Prepare data set for survival analysis
#'
#' @param nif A nif data set.
#' @param analyte The analyte as character.
#' @param group The grouping variable, as character.
#'
#' @returns A data frame.
#' @keywords internal
#' @noRd
make_surv_dataset <- function(nif, analyte, group = NULL) {
  nif %>%
    as.data.frame() %>%
    filter(.data$TAFD >= 0) %>%
    filter(.data$ANALYTE == analyte) %>%
    filter(.data$EVID == 0) %>%
    mutate(TIMED = .data$TAFD/24) %>%
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
}


#' Kaplan-Meier plot for events
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param nif A nif object.
#' @param analyte The analyte.
#' @param dose The dose, defaults to all doses, if NULL.
#' @param group Grouping variable.
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
    ...
  ) {
  # Validate input is a NIF object
  if (!inherits(nif, "nif")) {
    stop("Input must be a NIF object")
  }

  if (is.null(dose)) {
    dose <- unique(filter(nif, .data$EVID == 0)$DOSE)
  }

  # if(is.null(group)) {
  #   group <- 1
  # }

  # temp <- nif %>%
  #   as.data.frame() %>%
  #   filter(
  #     .data$EVID == 0,
  #     .data$ANALYTE == analyte,
  #     .data$DOSE %in% dose
  #   ) %>%
  #   mutate(TIMED = .data$TAFD / 24)

  temp <- make_surv_dataset(
    filter(nif, .data$DOSE %in% dose),
    analyte,
    group)

  if(!is.null(group) & all(group %in% names(temp))) {
    temp <- temp %>%
      tidyr::unite(group, all_of(group))
  } else {
    temp <- temp %>%
      mutate(group = 1)
  }


  # sf <- survival::survfit(Surv(TIMED, DV) ~ group, data = temp)
  sf <- survival::survfit(Surv(time, status) ~ group, data = temp)

  if(!is.null(sf$strata)) {
    names(sf$strata) <- gsub("group=", "", names(sf$strata))
  }

  p <- survminer::ggsurvplot(sf, ...)
  legend <- nif::nice_enumeration(group)

  p$plot <- p$plot +
    ggplot2::labs(fill = legend, color = legend)

  if(is.null(sf$strata)) {
    p$plot <- p$plot +
      theme(legend.position = "none")
  }

  return(p)
}
