#' @title Get stars from a gicven pvalue.
#' @param x Numeric. Vector of p values.
#' @return Character.
#' @keywords internal
#' @importFrom stats symnum
starPValue <- function(x)
  {
  return (symnum(x, corr = FALSE, na = FALSE, legend = FALSE,
         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
         symbols = c("***", " **", "  *", "  .", " ns")))
}