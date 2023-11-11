
cut_labels <- function(breaks) {
  n <- length(breaks)
  labels <- character(n - 1)
  for (i in 1:(n - 1)) {
    labels[i] <- paste(breaks[i], breaks[i + 1] - 1, sep = "-")
  }
  return(labels)
}
