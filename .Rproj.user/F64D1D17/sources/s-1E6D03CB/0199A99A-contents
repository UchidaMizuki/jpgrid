# str_c_replace_na <- function(..., replacement = "") {
#   args <- list2(...) %>%
#     purrr::map(purrr::partial(stringr::str_replace_na,
#                               replacement = replacement))
#   exec(stringr::str_c, !!!args)
# }

na_if_na <- function(x, y) {
  dplyr::if_else(are_na(y),
                 vec_cast(NA, x),
                 x)
}
