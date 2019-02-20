
which.nth <- function(x, condition, degree, output) {
  names <- names(sort(table(x[condition]),
    decreasing = TRUE
  )[degree])

  count <- sum(sort(table(x[condition]),
    decreasing = TRUE
  )[degree], na.rm = TRUE)

  if (output == "names") {
    return(names)
  } else if (output == "count") {
    return(count)
  } else {
    print("Output not one of names or count")
  }
}
