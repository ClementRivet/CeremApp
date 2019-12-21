#' Normalize
#'
#' Allows to normalize signals
#'
#' @param x current value or current data frame
#'
#' @param minVal the minimum value you want
#'
#' @param maxVal the macimum value you want
#'
#' @export
#'
#' @examples \dontrun{
#'  wt <- dataset::mtcars$wt
#'  wt <- normalize(wt, 0, 1)
#'  wt
#'}
#'
normalize <- function(x, minVal, maxVal){
  minVal + maxVal*(x - min(x))/(max(x) - min(x))
}
