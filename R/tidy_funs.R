pluck_value <- function(x, field) {
  lapply(x, "[[", field)
}

add_commas <- function(num) {
  format(numbers, big.mark=",", scientific=FALSE, trim=TRUE)
}

forest_plot <- function(estimate, lci, uci, labels = NULL, rotate = FALSE) {
  assertthat::assert_that(is.numeric(estimate) && length(estimate) > 0)
  assertthat::assert_that((is.numeric(lci) && length(lci) > 0) && (is.numeric(uci) && length(uci) > 0))
  assertthat::assert_that(length(lci) == length(uci) && length(lci) == length(estimate))
  if(is.null(labels)) labels <- seq_along(estimate)

  data <- cbind(estimate, lci, uci, labels)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = labels, y = estimate, ymin = lci, ymax = uci)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar()
}



