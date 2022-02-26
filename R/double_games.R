#' Double Games
#'
#' @param df of you want to load the data this can be ignored
#' @param load_data if you want to load the data from github. The default is FALSE
#'
#' @description Helper function that takes input where each game has one row with teams as `away_team` and `home_team` and
#' returns with each game having two rows with teams listed as `team` and `opp`
#' @return df
#' @export
#'

double_games <- function(df, load_data = FALSE){

  if (isTRUE(load_data)) {
    outcomes <- readr::read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv")
  }
  if (isFALSE(load_data)) {
    outcomes <- df
  }

  outcomes <- outcomes %>%
    dplyr::mutate(dplyr::across(
      c(.data$home_team, .data$away_team),
      ~ stringr::str_replace_all(., c(
        "JAC" = "JAX",
        "STL" = "LA",
        "SL" = "LA",
        "ARZ" = "ARI",
        "BLT" = "BAL",
        "CLV" = "CLE",
        "HST" = "HOU",
        "SD" = "LAC",
        "OAK" = "LV"
      ))
    ))

  outcomes <- outcomes %>%
    dplyr::transmute(
      .data$season,
      .data$week,
      game_date = .data$gameday,
      .data$game_id,
      .data$home_team,
      .data$away_team,
      .data$home_score,
      .data$away_score,
      team = .data$away_team,
      opponent = .data$home_team,
      points_for = .data$away_score,
      points_against = .data$home_score,
      point_differential = -.data$result
    ) %>%
    dplyr::bind_rows(., .data$outcomes %>%
                       dplyr::transmute(
                         .data$season,
                         .data$week,
                         game_date = .data$gameday,
                         .data$game_id,
                         .data$home_team,
                         .data$away_team,
                         .data$home_score,
                         .data$away_score,
                         team = .data$home_team,
                         opponent = .data$away_team,
                         points_for = .data$home_score,
                         points_against = .data$away_score,
                         point_differential = .data$result
                       )) %>%
    dplyr::mutate(
      win = dplyr::if_else(.data$point_differential > 0, 1, 0),
      winner = dplyr::if_else(.data$point_differential > 0, .data$team, .data$opponent),
      loser = dplyr::if_else(.data$point_differential < 0, .data$team, .data$opponent)
    ) %>%
    dplyr::arrange(.data$season, .data$week) %>%
    dplyr::group_by(.data$season, .data$team) %>%
    dplyr::mutate(game_number = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(c(.data$game_number, .data$team, .data$opponent, .data$winner, .data$loser), .after = game_id)
}
