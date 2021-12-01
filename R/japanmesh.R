#' Functions for the Japanese Regional Mesh Codes (JIS X 0410)
#'
#' japanmesh is an R package for using the reference regional mesh (the 1st mesh to the 3rd mesh),
#' the split regional mesh as defined by the JIS (Japan Industrial Standard) X 0410 'regional mesh code'
#' and 1/10 subdivision of the 3rd mesh.
#' Regional mesh codes are square-like regional divisions set up for all regions of Japan based on longitude and latitude.
#'
#' @seealso <https://www.jisc.go.jp/app/jis/general/GnrJISNumberNameSearchList?show&jisStdNo=X0410>
#' @seealso <https://www.stat.go.jp/data/mesh/pdf/gaiyo1.pdf>
#'
#' @import vctrs
#' @import rlang
#' @importFrom magrittr %>%
#'
#' @name japanmesh
"_PACKAGE"

#' @name mesh
#' @param mesh A \code{mesh} vector.
NULL

#' @name mesh_to
#' @param mesh A \code{mesh} vector or a list of \code{mesh} vector.
#' @param mesh_to A \code{mesh} vector.
#' @param close Should the path of each element be closed when \code{mesh} is a list?
NULL

#' @name size
#' @param size A mesh size.
NULL
