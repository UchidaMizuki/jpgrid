#' Functions for the Japanese Regional grid Codes (JIS X 0410)
#'
#' japangrid is an R package for using the reference regional grid (the 1st grid to the 3rd grid),
#' the split regional grid as defined by the JIS (Japan Industrial Standard) X 0410 'regional grid code'
#' and 1/10 subdivision of the 3rd grid.
#' Regional grid codes are square-like regional divisions set up for all regions of Japan based on longitude and latitude.
#'
#' @seealso <https://www.jisc.go.jp/app/jis/general/GnrJISNumberNameSearchList?show&jisStdNo=X0410>
#' @seealso <https://www.stat.go.jp/data/grid/pdf/gaiyo1.pdf>
#'
#' @import vctrs
#' @import rlang
#' @importFrom magrittr %>%
#'
#' @name japangrid
"_PACKAGE"

#' @name grid
#' @param grid A \code{grid} vector.
NULL

#' @name grid_to
#' @param grid A \code{grid} vector or a list of \code{grid} vector.
#' @param grid_to A \code{grid} vector.
#' @param close Should the path of each element be closed when \code{grid} is a list?
NULL

#' @name size
#' @param size A grid size.
NULL
