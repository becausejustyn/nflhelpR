#' Load NGS
#'
#' @param stat_type either passing, receiving or rushing
#' @param year the year you want the stats from. Starts from 2016. Default is current year.
#' @param weekly if you want the weekly or season stats. Default is yes.
#' @param regular_season do you want to exclude postseason? Default is yes.
#' @param join_roster do you want to join to pbp data? Default is no.
#' @param pbp_data if you want to join with pbp data, define what your pbp is called.
#'
#' @return
#' @export
#'
#' @examples load_ngs(stat_type = "rushing")

load_ngs <- function(
  stat_type = c("passing", "receiving", "rushing"), year = c(2021),
  weekly = TRUE, regular_season = TRUE, join_roster = FALSE, pbp_data = pbp_data
){

  stat_type <- rlang::arg_match0(stat_type, c("passing", "receiving", "rushing"))

  ngs <- read_rds(glue::glue("~/Documents/nfl/data/ngs/ngs_{stat_type}.rds"))

  if (isTRUE(weekly)) {
    ngs <- dplyr::filter(ngs, .data$week >= 1, .data$season == year)
  }
  if (isFALSE(weekly)) {
    ngs <- dplyr::filter(ngs, .data$week == 0, .data$season == year)
  }

  # if true, drop postseason games
  if (isTRUE(regular_season)) {
    ngs <- dplyr::filter(ngs, .data$season_type == 'REG')
  }

  # if true, join the ngs data to pbp
  if (isTRUE(join_roster)) {

    ngs <- pbp_data %>%
      dplyr::left_join(ngs, by = c("jersey_number" = "player_jersey_number", "posteam" = "team_abbr", "season", "name" = "player_short_name"))


  }


  return(ngs)
}

