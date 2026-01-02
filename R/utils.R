# Helper function to validate character string parameters
validate_char_param <- function(
    param,
    param_name,
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE) {
  if(any(is.na(param))) {
    stop(paste0(param_name, " must not contain NA"))
  }

  if (allow_null && is.null(param)) {
    return(invisible(NULL))
  }

  if(
    is.null(param) ||
    !is.character(param) ||
    (length(param) != 1 && !allow_multiple)) {
    stop(paste0(param_name, " must be a single character string"))
  }

  if (!allow_empty && all(nchar(param) == 0)) {
    stop(paste0(param_name, " must be a non-empty character string"))
  }

  return(invisible(NULL))
}


# Helper function to validate logical parameters
validate_logical_param <- function(
    param,
    param_name,
    allow_null = FALSE,
    allow_multiple = FALSE) {
  if(any(is.na(param))) {
    stop(paste0(param_name, " must not contain NA"))
  }

  if (allow_null && is.null(param)) {
    return(invisible(NULL))
  }

  if(
    is.null(param) ||
    !is.logical(param) ||
    (length(param) != 1 && !allow_multiple)) {
    stop(paste0(param_name, " must be a single logical value"))
  }

  return(invisible(NULL))
}


# Helper function to validate numeric parameters
validate_numeric_param <- function(
    param,
    param_name,
    allow_null = FALSE,
    allow_na = FALSE,
    allow_multiple = FALSE) {

  # if(any(is.na(param))) {
  #   stop(paste0(param_name, " must not contain NA"))
  # }

  if (allow_null && is.null(param)) {
    return(invisible(NULL))
  }

  if(
    is.null(param) ||
    !is.numeric(param) ||
    (length(param) != 1 && !allow_multiple)) {
    stop(paste0(param_name, " must be a single numeric value"))
  }

  return(invisible(NULL))
}


