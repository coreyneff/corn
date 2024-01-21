save_figure <- function(fig, path = NULL, fmt = NULL, ht = NULL, wdth = NULL, ...){
  formats <- c("svg", "png", "pdf", "tiff", "jpeg", "html")
  assertthat::assert_that(!is.null(path) && is.character(path) && length(path) > 0)
  assertthat::assert_that(inherits(fig, "ggplot"))

  path <- tools::file_path_sans_ext(path)
  fmt <- fmt[fmt %in% formats]
  if(is.null(fmt) || !is.character(fmt) || length(format) == 0L) {
    fmt <- "pdf"
  }
  outfiles <- unlist(lapply(path, \(x) paste0(x, paste0(".", fmt))))
  for(file in outfiles){
    ggsave(plot = fig, filename = file, width = wdth, height = ht, ...)
  }
}
