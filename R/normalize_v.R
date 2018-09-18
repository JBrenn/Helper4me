#' @title min-max normalization
#' @description min-max normalization of vector
#' @param x data vector
#' @return normalised data vector
#' @details
#' @examples
#' normalize_v(1:10)
#' @rdname normalize_v
#' @export
normalize_v <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}
