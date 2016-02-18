#' Calculate the standard error.
#'
#' Calculates the standard error of numbers in a vector. NAs are removed.
#' @param x numeric vector. NAs are allowed but will be removed before calculation.
#' @export
#' @examples
#' x <- c(2,4,3,5,NA,5)
#' sterr(x)

sterr <- function(x) sd(x, na.rm=T)/sqrt(length(na.omit(x)))

