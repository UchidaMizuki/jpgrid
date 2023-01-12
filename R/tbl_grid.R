#' Convert a data frame into a tbl_grid object
#'
#' The `tbl_grid` object is a data frame with `grid` objects in the columns.
#' `as_tbl_grid` converts a data frame into a tbl_grid object.
#'
#' @param x An object to be converted into an object class `tbl_grid`.
#' @param var A variable to specify the grid object. By default, the first
#' column of the grid object is taken.
#' @param size 	A grid size.
#' @param strict A logical scalar. Should the number of digits in the grid
#' square code match a given number of digits?
#' @param ... Additional arguments passed to [stickyr::new_sticky_tibble()]
#'
#' @return A `tbl_grid` object.
#'
#' @export
as_tbl_grid <- function(x,
                        var = NULL,
                        size = NULL,
                        strict = TRUE, ...) {
  if (quo_is_null(enquo(var))) {
    are_grid <- purrr::map_lgl(x, is_grid)

    if (any(are_grid)) {
      var <- names(x)[[which(are_grid)[[1L]]]]
    } else {
      abort("`x` must have a column of `grid` class.")
    }
  } else {
    var <- tidyselect::vars_pull(names(x), {{ var }})
  }

  out <- x %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(var),
                                purrr::partial(grid_impl,
                                               strict = strict,
                                               size = size)))

  stickyr::new_sticky_tibble(out,
                             cols = !!var, ...,
                             class = "tbl_grid",
                             class_grouped_df = "tbl_grid",
                             class_rowwise_df = "tbl_grid")
}

#' @export
tbl_sum.tbl_grid <- function(x) {
  out <- NextMethod()
  names(out)[[1L]] <- "A tbl_grid"
  names(out)[[2L]] <- "Grid column"
  out
}
