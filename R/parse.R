#' Parse grid square codes
#'
#' @param x A character vector of grid square codes.
#' @param grid_size A grid size.
#' @param strict A scalar logical. Should the number of digits in the grid
#' square code match a given number of digits? By default, `TRUE`.
#'
#' @examples
#' parse_grid("53394526313")
#' parse_grid("53394526313", "80km")
#' parse_grid("53394526313", "80km",
#'            strict = FALSE)
#'
#' @export
parse_grid <- function(x, grid_size = NULL, strict = TRUE) {
  if (is.null(grid_size)) {
    pattern <- stringr::str_c(
      "^",
      stringr::str_dup("(<\\-?\\d+>|\\d{2})", 2),
      "(\\d*)"
    )
    code_others <- x |>
      stringr::str_match(pattern) |>
      tibble::as_tibble(
        .name_repair = ~ c("code", "code_Y_80km", "code_X_80km", "code_others")
      ) |>
      tidyr::drop_na(code_others) |>
      dplyr::pull("code_others")

    digit <- min(stringr::str_length(code_others))

    if (digit %in% 0L:1L) {
      grid_size <- 80000L
    } else if (digit == 2L) {
      grid_size <- 10000L
    } else if (digit == 3L) {
      grid_size <- 5000L
    } else if (digit == 4L) {
      grid_size <- 1000L
    } else if (digit == 5L) {
      grid_size <- 500L
    } else if (digit == 6L) {
      is_grid_size_250m <- code_others |>
        stringr::str_extract("\\d{6}") |>
        stringr::str_ends("[1-4]{2}")
      if (all(is_grid_size_250m)) {
        grid_size <- 250L
      } else {
        grid_size <- 100L
      }
    } else if (digit >= 7L) {
      grid_size <- 125L
    }

    grid_size_name <- switch(
      as.character(grid_size),
      `80000` = "80km",
      `10000` = "10km",
      `5000` = "5km",
      `1000` = "1km",
      `500` = "500m",
      `250` = "250m",
      `125` = "125m",
      `100` = "100m"
    )

    cli_inform("Guessing, grid_size = {.val {grid_size_name}}")
  } else {
    grid_size <- grid_size_match(grid_size)
  }

  pattern_80km <- stringr::str_c(
    "^",
    stringr::str_dup("(<\\-*\\d+>|\\d{2})", 2L)
  )

  code_80km <- c("code_Y_80km", "code_X_80km")
  code_10km <- c("code_Y_10km", "code_X_10km")
  code_1km <- c("code_Y_1km", "code_X_1km")
  code_100m <- c("code_Y_100m", "code_X_100m")

  if (grid_size == 80000L) {
    digit <- 0L
    name <- code_80km
  } else if (grid_size == 10000L) {
    digit <- 2L
    name <- c(code_80km, code_10km)
  } else if (grid_size == 5000L) {
    digit <- 3L
    name <- c(code_80km, code_10km, "code_5km")
  } else if (grid_size == 1000L) {
    digit <- 4L
    name <- c(code_80km, code_10km, code_1km)
  } else if (grid_size == 500L) {
    digit <- 5L
    name <- c(code_80km, code_10km, code_1km, "code_500m")
  } else if (grid_size == 250L) {
    digit <- 6L
    name <- c(code_80km, code_10km, code_1km, "code_500m", "code_250m")
  } else if (grid_size == 125L) {
    digit <- 7L
    name <- c(
      code_80km,
      code_10km,
      code_1km,
      "code_500m",
      "code_250m",
      "code_125m"
    )
  } else if (grid_size == 100L) {
    digit <- 6L
    name <- c(code_80km, code_10km, code_1km, code_100m)
  }

  if (strict) {
    strict <- "$"
  } else {
    strict <- ""
  }

  pattern <- stringr::str_c(
    pattern_80km,
    stringr::str_dup("(\\d)", digit),
    strict
  )
  args <- x |>
    stringr::str_match(pattern) |>
    tibble::as_tibble(.name_repair = ~ c("code", name)) |>
    dplyr::select(!"code")

  exec(code_to_grid, grid_size = grid_size, !!!args)
}
