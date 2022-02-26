#' Calculate Weekly EPA
#' @param pbp play by play data from nflfastR
#'
#' @return Returns weekly epa/play for each team.
#' @export
#' @importFrom rlang .data

weekly_epa <- function(pbp){
  pbp <- pbp %>%
    dplyr::filter(
      !is.na(.data$epa),
      !is.na(.data$ep),
      !is.na(.data$posteam),
      .data$play_type %in% c("pass", "run")
    ) %>%
    dplyr::group_by(.data$game_id, .data$season, .data$week, .data$posteam, .data$home_team) %>%
    dplyr::summarise(
      off_epa = mean(.data$epa),
    ) %>%
    dplyr::left_join(
      dplyr::filter(pbp,
                    !is.na(.data$epa),
                    !is.na(.data$ep),
                    !is.na(.data$posteam),
                    .data$play_type %in% c("pass", "run")
      ) %>%
        dplyr::group_by(.data$game_id, .data$season, .data$week, .data$defteam, .data$away_team) %>%
        dplyr::summarise(def_epa = mean(.data$epa)),
      by = c("game_id", "posteam" = "defteam", "season", "week"),
      all.x = TRUE) %>%
    dplyr::mutate(opponent = dplyr::if_else(.data$posteam == .data$home_team, .data$away_team, .data$home_team)) %>%
    dplyr::select(game_id, season, week, home_team, away_team, posteam, opponent, off_epa, def_epa)
}


