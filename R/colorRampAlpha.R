#' @title color ramp palette with transparency
#' @description color ramp palette with transparency
#' @param ... arguments to pass to \code{\link[grDevices]{colorRampPalette}}.
#' @param n number of colors in palette
#' @param alpha alpha channel (opacity) value [0;1]
#' @return colorRampAlpha returns a vector of defined colors with transparency interpolating the given sequence.
#' @details for details see \code{\link[grDevices]{colorRamp}} and \code{\link[grDevices]{colorRampPalette}}
#' @examples
#' library(RColorBrewer)
#' colorRampAlpha(brewer.pal(9,"PuOr"), n = 10, alpha=.5)
#' @rdname colorRampAlpha
#' @export
colorRampAlpha <- function(..., n, alpha) {
  colors <- colorRampPalette(...)(n)
  paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}
