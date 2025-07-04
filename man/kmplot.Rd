% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_plot_surv.R
\name{kmplot}
\alias{kmplot}
\title{Kaplan-Meier plot for events}
\usage{
kmplot(nif, analyte, dose = NULL, group = NULL, ...)
}
\arguments{
\item{nif}{A nif object.}

\item{analyte}{The analyte.}

\item{dose}{The dose, defaults to all doses, if NULL.}

\item{group}{Grouping variable.}

\item{...}{
  Arguments passed on to \code{\link[survminer:ggsurvplot]{survminer::ggsurvplot}}
  \describe{
    \item{\code{conf.int}}{logical value. If TRUE, plots confidence interval.}
    \item{\code{pval}}{logical value, a numeric or a string. If logical and TRUE, the
p-value is added on the plot. If numeric, than the computet p-value is
substituted with the one passed with this parameter. If character, then the
customized string appears on the plot. See examples - Example 3.}
    \item{\code{surv.median.line}}{character vector for drawing a horizontal/vertical
line at median survival. Allowed values include one of c("none", "hv", "h",
"v"). v: vertical, h:horizontal.}
    \item{\code{risk.table}}{Allowed values include: \itemize{ \item TRUE or FALSE
specifying whether to show or not the risk table. Default is FALSE. \item
"absolute" or "percentage". Shows the \bold{absolute number} and the
\bold{percentage} of subjects at risk by time, respectively. \item "abs_pct"
to show both absolute number and percentage. \item "nrisk_cumcensor" and
"nrisk_cumevents". Show the number at risk and, the cumulative number of
censoring and events, respectively. }}
  }}
}
\value{
A ggplot object.
}
\description{
`r lifecycle::badge("experimental")`
}
