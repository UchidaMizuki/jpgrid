#' Neighborhood grid square codes (Deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param grid A `grid` vector.
#' @param n A numeric vector of degrees. By default, `1L`.
#' @param moore Moore neighborhood (`TRUE`, default) or Von Neumann neighborhood
#' (`FALSE`).
#' @param simplify Should simplify the format of the return?
#'
#' @return A list of `grid` vectors.
#'
#' @export
grid_neighbor <- function(grid, n = 1L, moore = TRUE, simplify = TRUE) {
  lifecycle::deprecate_warn(
    "0.5.0",
    what = "grid_neighbor()",
    details = 'Please use `grid_neighborhood(type = "moore")`'
  )

  grid_neighborhood(
    grid,
    n = n,
    type = if (moore) "moore" else "von_neumann",
    simplify = simplify
  )
}
