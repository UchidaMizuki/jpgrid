#' @export
grid_convert <- function(x, size) {
  size <- size_match(size)
  ratio <- size / grid_size(x)

  stopifnot(ratio %% 1L == 0L)
  ratio <- as.integer(ratio)

  new_grid(size = size,
           n_X = field(x, "n_X") %/% ratio,
           n_Y = field(x, "n_Y") %/% ratio)
}
