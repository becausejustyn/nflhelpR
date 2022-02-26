#' Rolling SSE Difference
#'
#' @param x tot.withinss column
#' @param n amount to lag
#'
#' @return numerical values



rolling_sse_difference <- function(x, n = 2){
  x - n * dplyr::lead(x) + dplyr::lead(x, n)
}
