fill.na <- function(x) {
  lapply(x, function(y) {
    length(y) <- max(lengths(x))
    y
  })
}
