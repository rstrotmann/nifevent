
#' Validate single logical value
#'
#' @param x The test value.
#' @param label A label, e.g., the variable name for x.
#' @param allow_null Allow x to be NULL, as logical.
#'
#' @returns Nothing.
validate_slv <- function(x, label = NULL, allow_null = FALSE) {
  if(is.null(label)) {
    label <- as.character(x)
  }
  if(!is.logical(x) || length(x) != 1) {
    if(!(allow_null = TRUE & is.null(x)))
      stop(paste0(label, " must be a single logical value"))
  }
}


#' Validate single character value
#'
#' @param x The test value.
#' @param label A label, defaults to the value of x.
#' @param allow_null Allow x to be NULL, as logical.
#'
#' @returns Nothing.
validate_scv <- function(x, label = NULL, allow_null = FALSE) {
  if(is.null(label)) {
    label <- as.character(x)
  }
  if(allow_null == TRUE & is.null(x)){
    return()
  }
  if(!is.character(x) || length(x) != 1) {
    stop(paste0(label, " must be a single character string"))
  }
}
