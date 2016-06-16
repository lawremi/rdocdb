## uses c() to combine high-level classes like Date
simplify2array2 <- function(x) {
  x[vapply(x, is.null, logical(1L))] <- NA # somewhat debatable
  uniq.lengths <- unique(lengths(x))
  if (length(uniq.lengths) != 1L) {
    x
  } else if (uniq.lengths == 1L) {
    do.call(c, x)
  } else {
    simplify2array(x, higher=FALSE)
  }
}
