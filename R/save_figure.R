save_figure <- function(
  fig,
  path,
  fmt = NULL,
  ht = NULL,
  wdth = NULL,
  units = "in",
  dpi = 300,
  archive = TRUE,
  archive_dir = "Archive",
  overwrite = FALSE,
  create_dirs = TRUE,
  archive_stamp = "%Y%m%d_%H%M%S",
  html_selfcontained = TRUE,
  html_args = list(),
  verbose = TRUE,
  ...
) {
  formats <- c("svg", "png", "pdf", "tiff", "tif", "jpeg", "jpg", "html")

  # Basic checks
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }

  if (!inherits(fig, "ggplot")) {
    stop("'fig' must inherit from class 'ggplot'.", call. = FALSE)
  }

  if (missing(path) || is.null(path) || !is.character(path) || length(path) == 0L) {
    stop("'path' must be a non-empty character vector.", call. = FALSE)
  }

  if (anyNA(path) || any(!nzchar(trimws(path)))) {
    stop("'path' cannot contain NA or empty strings.", call. = FALSE)
  }

  if (!is.logical(archive) || length(archive) != 1L || is.na(archive)) {
    stop("'archive' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(create_dirs) || length(create_dirs) != 1L || is.na(create_dirs)) {
    stop("'create_dirs' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(archive_dir) || length(archive_dir) != 1L ||
      is.na(archive_dir) || !nzchar(archive_dir)) {
    stop("'archive_dir' must be a single non-empty string.", call. = FALSE)
  }

  if (grepl("^([A-Za-z]:)?[/\\\\]", archive_dir)) {
    stop("'archive_dir' must be a relative folder name, such as 'Archive'.", call. = FALSE)
  }

  units <- match.arg(units, c("in", "cm", "mm", "px"))

  if (!is.null(wdth) &&
      (!is.numeric(wdth) || length(wdth) != 1L || !is.finite(wdth) || wdth <= 0)) {
    stop("'wdth' must be NULL or a single positive number.", call. = FALSE)
  }

  if (!is.null(ht) &&
      (!is.numeric(ht) || length(ht) != 1L || !is.finite(ht) || ht <= 0)) {
    stop("'ht' must be NULL or a single positive number.", call. = FALSE)
  }

  if (!is.numeric(dpi) && !is.character(dpi)) {
    stop("'dpi' must be numeric or a valid ggplot2 dpi string.", call. = FALSE)
  }

  if (!is.list(html_args)) {
    stop("'html_args' must be a list.", call. = FALSE)
  }

  # Remove a known figure extension from path, but do not strip arbitrary suffixes.
  strip_known_ext <- function(x) {
    ext <- tolower(tools::file_ext(x))
    known <- nzchar(ext) & ext %in% formats

    out <- x
    out[known] <- sub("\\.[^./\\\\]+$", "", out[known])
    out
  }

  # Determine output files
  fmt_supplied <- !missing(fmt) && !is.null(fmt)

  if (fmt_supplied) {
    if (!is.character(fmt)) {
      stop("'fmt' must be NULL or a character vector.", call. = FALSE)
    }

    fmt <- fmt[!is.na(fmt)]
    fmt <- tolower(trimws(fmt))
    fmt <- sub("^\\.", "", fmt)
    fmt <- unique(fmt[nzchar(fmt)])

    invalid_fmt <- setdiff(fmt, formats)

    if (length(invalid_fmt) > 0L) {
      warning(
        "Ignoring unsupported format(s): ",
        paste(invalid_fmt, collapse = ", "),
        call. = FALSE
      )
    }

    fmt <- intersect(fmt, formats)

    if (length(fmt) == 0L) {
      warning("No valid formats supplied. Defaulting to 'pdf'.", call. = FALSE)
      fmt <- "pdf"
    }

    base_paths <- strip_known_ext(path)

    outfiles <- unlist(
      lapply(base_paths, function(x) paste0(x, ".", fmt)),
      use.names = FALSE
    )
  } else {
    ext <- tolower(tools::file_ext(path))
    base_paths <- strip_known_ext(path)

    fmt_by_path <- ifelse(nzchar(ext) & ext %in% formats, ext, "pdf")
    outfiles <- paste0(base_paths, ".", fmt_by_path)
  }

  outfiles <- unique(outfiles)

  # Create parent directories if needed
  parent_dirs <- unique(dirname(outfiles))
  missing_dirs <- parent_dirs[!dir.exists(parent_dirs)]

  if (length(missing_dirs) > 0L) {
    if (!create_dirs) {
      stop(
        "Output directory does not exist: ",
        paste(missing_dirs, collapse = ", "),
        call. = FALSE
      )
    }

    for (d in missing_dirs) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Archive helper
  archive_existing_file <- function(file) {
    if (!file.exists(file)) {
      return(NA_character_)
    }

    this_archive_dir <- file.path(dirname(file), archive_dir)

    if (!dir.exists(this_archive_dir)) {
      dir.create(this_archive_dir, recursive = TRUE, showWarnings = FALSE)
    }

    stamp <- format(Sys.time(), archive_stamp)
    ext <- tools::file_ext(file)
    stem <- tools::file_path_sans_ext(basename(file))

    make_candidate <- function(i = NULL) {
      suffix <- if (is.null(i)) {
        stamp
      } else {
        paste0(stamp, "_", sprintf("%02d", i))
      }

      if (nzchar(ext)) {
        file.path(this_archive_dir, paste0(stem, "_", suffix, ".", ext))
      } else {
        file.path(this_archive_dir, paste0(stem, "_", suffix))
      }
    }

    archived_file <- make_candidate()
    i <- 1L

    while (file.exists(archived_file)) {
      archived_file <- make_candidate(i)
      i <- i + 1L
    }

    ok <- file.rename(file, archived_file)

    if (!ok) {
      stop(
        "Could not move existing file to archive: ",
        file,
        call. = FALSE
      )
    }

    archived_file
  }

  # Save helpers
  save_static <- function(file) {
    dots <- list(...)

    base_args <- list(
      filename = file,
      plot = fig
    )

    optional_args <- list(
      width = wdth,
      height = ht,
      units = units,
      dpi = dpi
    )

    optional_args <- optional_args[
      !vapply(optional_args, is.null, logical(1))
    ]

    conflicts <- intersect(names(optional_args), names(dots))

    if (length(conflicts) > 0L) {
      warning(
        "Arguments in '...' override these explicit arguments: ",
        paste(conflicts, collapse = ", "),
        call. = FALSE
      )
      optional_args[conflicts] <- NULL
    }

    do.call(
      ggplot2::ggsave,
      c(base_args, optional_args, dots)
    )
  }

  save_html <- function(file) {
    if (!requireNamespace("plotly", quietly = TRUE) ||
        !requireNamespace("htmlwidgets", quietly = TRUE)) {
      stop(
        "Saving HTML requires packages 'plotly' and 'htmlwidgets'. ",
        "Install them or remove 'html' from 'fmt'.",
        call. = FALSE
      )
    }

    widget <- plotly::ggplotly(fig)

    args <- c(
      list(
        widget = widget,
        file = file,
        selfcontained = html_selfcontained
      ),
      html_args
    )

    do.call(htmlwidgets::saveWidget, args)
  }

  archived_to <- rep(NA_character_, length(outfiles))

  # Save files
  for (i in seq_along(outfiles)) {
    file <- outfiles[[i]]

    if (file.exists(file)) {
      if (archive) {
        archived_to[[i]] <- archive_existing_file(file)
      } else if (!overwrite) {
        stop(
          "File already exists and would be overwritten: ",
          file,
          "\nUse archive = TRUE or overwrite = TRUE.",
          call. = FALSE
        )
      }
    }

    ext <- tolower(tools::file_ext(file))

    if (identical(ext, "html")) {
      save_html(file)
    } else {
      save_static(file)
    }
  }

  result <- data.frame(
    file = outfiles,
    format = tolower(tools::file_ext(outfiles)),
    archived_to = archived_to,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message(
      "Saved ",
      nrow(result),
      " file(s):\n",
      paste(result$file, collapse = "\n")
    )
  }

  invisible(result)
}
