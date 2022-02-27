#' Load PBP Data
#'
#' @param seasons what years do you want to load
#'
#' @return
#' @export
#'
#' @examples get_pbp_data(seasons = 2021)

get_pbp_data <- function(seasons = c(2021)){

  pbp <- purrr::map_df(c(seasons), function(x) {
    readr::read_rds(
      glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
    )
  })

}
