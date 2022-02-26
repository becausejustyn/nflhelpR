#' Height to cm
#' @description convert character string of height e.g. "6-4" to a numerical
#' value in cm.   Note: it will break if the height is above 10feet.
#' @param x height character value
#'
#' @return numerical value in cm
#' @export
#'
#' @examples height_to_cm("6-4")

height_to_cm <- function(x) {
  feet = substr(x, 1, 1) %>% as.numeric()
  inches = substr(x, 3, 4) %>% as.numeric()
  cm = (feet * 30.48) + (inches * 2.54)
  return(cm)
}
