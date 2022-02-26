#' Calculate Harmonic Mean
#'
#' @param x numerical values
#' @param na.rm remove NA values
#'
#' @return harmonic mean value
#' @export
#'
#' @examples faithful[['waiting']] %>% harmonic_mean()

harmonic_mean <- function(x, na.rm = TRUE){
  1 / mean(1 / x)
}
