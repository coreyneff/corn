tabler <- function(data, ...,
                             by = NULL,
                             overall_name = "Total",
                             digits = 2,
                             percent_digits = 2,
                             p_digits = 3,
                             p_less_than = NULL,
                             zero_percent = "--",
                             missing_percent = "",
                             pvalue_na = "Unable to calculate",
                             pvalue_col = NULL,
                             repeat_pvalue = TRUE,
                             label_missing = "",
                             trim_range_zeros = TRUE,
                             force_categorical = NULL,
                             force_numeric = NULL,
                             numeric_as_categorical_cutoff = NULL,
                             fisher_threshold = 5,
                             fisher_simulate = NULL,
                             fisher_B = 2000,
                             chisq_correct = FALSE,
                             numeric_var_equal = FALSE,
                             sort_character_levels = FALSE) {
  required_packages <- c("rlang", "tidyselect", "tibble")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Install required package(s): ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  var_quos <- rlang::enquos(..., .ignore_empty = "all")
  if (length(var_quos) == 0) {
    stop("Supply at least one table variable in `...`.", call. = FALSE)
  }

  selected_vars <- tidyselect::eval_select(rlang::expr(c(!!!var_quos)), data)
  table_vars <- names(selected_vars)

  by_quo <- rlang::enquo(by)
  has_by <- !rlang::quo_is_null(by_quo)
  by_var <- NULL

  if (has_by) {
    selected_by <- tidyselect::eval_select(rlang::expr(c(!!by_quo)), data)
    if (length(selected_by) != 1) {
      stop("`by` must select exactly one variable.", call. = FALSE)
    }
    by_var <- names(selected_by)[[1]]
    if (is.null(pvalue_col)) {
      pvalue_col <- paste0(by_var, " p")
    }
  }

  p_cut <- if (is.null(p_less_than)) 10^(-p_digits) else p_less_than

  fmt_num <- function(x, ndigits = digits, trim = FALSE) {
    if (length(x) == 0 || is.na(x) || !is.finite(x)) {
      return("")
    }
    out <- formatC(x, format = "f", digits = ndigits)
    if (isTRUE(trim)) {
      out <- sub("\\.?0+$", "", out)
      if (identical(out, "-0")) out <- "0"
    }
    out
  }

  fmt_count <- function(x) {
    if (length(x) == 0 || is.na(x)) return("")
    as.character(as.integer(x))
  }

  fmt_percent <- function(n, denom) {
    if (length(n) == 0 || length(denom) == 0 || is.na(n) || is.na(denom) || denom <= 0) {
      return(missing_percent)
    }
    if (n == 0) {
      return(zero_percent)
    }
    paste0(formatC(100 * n / denom, format = "f", digits = percent_digits), "%")
  }

  fmt_pvalue <- function(p) {
    if (length(p) == 0 || is.na(p) || !is.finite(p)) {
      return(pvalue_na)
    }
    if (p < p_cut) {
      return(paste0("<", fmt_num(p_cut, ndigits = p_digits, trim = FALSE)))
    }
    fmt_num(p, ndigits = p_digits, trim = FALSE)
  }

  fmt_range <- function(lo, hi) {
    if (length(lo) == 0 || length(hi) == 0 || is.na(lo) || is.na(hi)) {
      return("")
    }
    paste0(
      fmt_num(lo, ndigits = digits, trim = trim_range_zeros),
      "-",
      fmt_num(hi, ndigits = digits, trim = trim_range_zeros)
    )
  }

  first_nonmissing_levels <- function(x, sort_levels = FALSE) {
    vals <- unique(as.character(x[!is.na(x)]))
    if (isTRUE(sort_levels)) vals <- sort(vals)
    vals
  }

  get_var_label <- function(x) {
    lab <- attr(x, "label", exact = TRUE)
    if (is.null(lab)) lab <- attr(x, "var_label", exact = TRUE)
    if (is.null(lab) || length(lab) == 0 || is.na(lab[[1]]) || identical(as.character(lab[[1]]), "")) {
      return(label_missing)
    }
    as.character(lab[[1]])
  }

  labelled_to_factor <- function(x) {
    labels <- attr(x, "labels", exact = TRUE)
    label_values <- unname(labels)
    label_names <- names(labels)

    if (is.null(label_names) || any(label_names == "")) {
      label_names <- as.character(label_values)
    }

    ord <- order(label_values)
    label_values <- label_values[ord]
    label_names <- label_names[ord]

    base_x <- as.vector(x)
    out <- rep(NA_character_, length(base_x))

    for (i in seq_along(label_values)) {
      out[!is.na(base_x) & base_x == label_values[[i]]] <- label_names[[i]]
    }

    unlabelled <- !is.na(base_x) & is.na(out)
    if (any(unlabelled)) {
      out[unlabelled] <- as.character(base_x[unlabelled])
    }

    levels_out <- unique(c(label_names, first_nonmissing_levels(out, sort_levels = FALSE)))
    factor(out, levels = levels_out)
  }

  to_display_factor <- function(x, is_by = FALSE) {
    if (!is.null(attr(x, "labels", exact = TRUE))) {
      return(labelled_to_factor(x))
    }

    if (is.factor(x)) {
      return(x)
    }

    if (is.logical(x)) {
      return(factor(as.character(x), levels = c("FALSE", "TRUE")))
    }

    values <- as.character(x)
    levels_out <- first_nonmissing_levels(values, sort_levels = isTRUE(sort_character_levels) && !is_by)
    factor(values, levels = levels_out)
  }

  is_categorical_var <- function(x, var_name) {
    if (!is.null(force_categorical) && var_name %in% force_categorical) {
      return(TRUE)
    }
    if (!is.null(force_numeric) && var_name %in% force_numeric) {
      return(FALSE)
    }
    if (is.factor(x) || is.character(x) || is.logical(x) || !is.null(attr(x, "labels", exact = TRUE))) {
      return(TRUE)
    }
    if (is.numeric(x) && !is.null(numeric_as_categorical_cutoff)) {
      return(length(unique(x[!is.na(x)])) <= numeric_as_categorical_cutoff)
    }
    FALSE
  }

  categorical_pvalue <- function(x_factor, by_factor) {
    keep <- !is.na(x_factor) & !is.na(by_factor)
    if (!any(keep)) return(NA_real_)

    x_keep <- droplevels(x_factor[keep])
    by_keep <- droplevels(by_factor[keep])

    if (nlevels(x_keep) < 2 || nlevels(by_keep) < 2) {
      return(NA_real_)
    }

    tab <- table(by_keep, x_keep)
    if (any(dim(tab) < 2) || sum(tab) == 0) {
      return(NA_real_)
    }

    chi <- tryCatch(
      suppressWarnings(stats::chisq.test(tab, correct = chisq_correct)),
      error = function(e) NULL
    )

    use_fisher <- is.null(chi) || !is.finite(chi$p.value)
    if (!is.null(fisher_threshold)) {
      use_fisher <- use_fisher || any(tab < fisher_threshold)
    }

    if (isTRUE(use_fisher)) {
      simulate <- if (is.null(fisher_simulate)) {
        nrow(tab) > 2 || ncol(tab) > 2
      } else {
        isTRUE(fisher_simulate)
      }

      fisher <- tryCatch(
        stats::fisher.test(tab, simulate.p.value = simulate, B = fisher_B),
        error = function(e) NULL
      )

      if (is.null(fisher) || !is.finite(fisher$p.value)) {
        return(NA_real_)
      }
      return(fisher$p.value)
    }

    chi$p.value
  }

  numeric_pvalue <- function(x, by_factor) {
    keep <- !is.na(x) & !is.na(by_factor)
    if (!any(keep)) return(NA_real_)

    test_data <- data.frame(
      y = x[keep],
      group = droplevels(by_factor[keep])
    )

    n_groups <- nlevels(test_data$group)
    if (n_groups < 2) {
      return(NA_real_)
    }

    p <- tryCatch(
      {
        if (n_groups == 2) {
          stats::t.test(y ~ group, data = test_data, var.equal = numeric_var_equal)$p.value
        } else {
          stats::oneway.test(y ~ group, data = test_data, var.equal = numeric_var_equal)$p.value
        }
      },
      error = function(e) NA_real_
    )

    p
  }

  make_empty_row_block <- function(var_name, label, responses) {
    tibble::tibble(
      Variable = rep(var_name, length(responses)),
      Label = rep(label, length(responses)),
      Response = responses
    )
  }

  categorical_table <- function(var_name) {
    x <- to_display_factor(data[[var_name]])
    label <- get_var_label(data[[var_name]])
    responses <- c("Valid N", levels(x))
    out <- make_empty_row_block(var_name, label, responses)

    group_labels <- overall_name
    by_factor <- NULL
    if (has_by) {
      by_factor <- to_display_factor(data[[by_var]], is_by = TRUE)
      group_labels <- c(group_labels, levels(by_factor))
    }

    for (group_i in seq_along(group_labels)) {
      group_label <- group_labels[[group_i]]
      if (group_i == 1) {
        idx <- !is.na(x)
      } else {
        idx <- !is.na(x) & !is.na(by_factor) & by_factor == group_label
      }

      denom <- sum(idx)
      counts <- if (length(levels(x)) == 0) {
        integer(0)
      } else {
        as.integer(table(factor(as.character(x[idx]), levels = levels(x))))
      }

      n_values <- c(fmt_count(denom), vapply(counts, fmt_count, character(1)))
      pct_values <- c("", vapply(counts, fmt_percent, character(1), denom = denom))

      out[[paste0(group_label, " N")]] <- n_values
      out[[paste0(group_label, " Percent")]] <- pct_values
    }

    if (has_by) {
      p_display <- fmt_pvalue(categorical_pvalue(x, by_factor))
      out[[pvalue_col]] <- if (isTRUE(repeat_pvalue)) {
        rep(p_display, nrow(out))
      } else {
        c(p_display, rep("", nrow(out) - 1))
      }
    }

    out
  }

  numeric_group_stats <- function(x, idx) {
    z <- x[idx]
    z <- z[!is.na(z)]
    n <- length(z)

    if (n == 0) {
      return(list(
        n = "0",
        mean = "",
        sd = "",
        median = "",
        iqr = "",
        range = "",
        q1_q3 = ""
      ))
    }

    q <- stats::quantile(z, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7)

    list(
      n = fmt_count(n),
      mean = fmt_num(mean(z), ndigits = digits, trim = FALSE),
      sd = if (n > 1) fmt_num(stats::sd(z), ndigits = digits, trim = FALSE) else "",
      median = fmt_num(stats::median(z), ndigits = digits, trim = FALSE),
      iqr = fmt_num(stats::IQR(z, na.rm = TRUE, type = 7), ndigits = digits, trim = FALSE),
      range = fmt_range(min(z), max(z)),
      q1_q3 = fmt_range(q[[1]], q[[2]])
    )
  }

  numeric_table <- function(var_name) {
    x <- data[[var_name]]
    label <- get_var_label(x)

    responses <- c("Valid N", "Mean (SD)", "Median (IQR)", "Range (25th-75th)")
    out <- make_empty_row_block(var_name, label, responses)

    group_labels <- overall_name
    by_factor <- NULL
    if (has_by) {
      by_factor <- to_display_factor(data[[by_var]], is_by = TRUE)
      group_labels <- c(group_labels, levels(by_factor))
    }

    for (group_i in seq_along(group_labels)) {
      group_label <- group_labels[[group_i]]
      if (group_i == 1) {
        idx <- !is.na(x)
      } else {
        idx <- !is.na(x) & !is.na(by_factor) & by_factor == group_label
      }

      stats <- numeric_group_stats(x, idx)

      out[[paste0(group_label, " N")]] <- c(
        stats$n,
        stats$mean,
        stats$median,
        stats$range
      )
      out[[paste0(group_label, " Percent")]] <- c(
        "",
        stats$sd,
        stats$iqr,
        stats$q1_q3
      )
    }

    if (has_by) {
      p_display <- fmt_pvalue(numeric_pvalue(x, by_factor))
      out[[pvalue_col]] <- if (isTRUE(repeat_pvalue)) {
        rep(p_display, nrow(out))
      } else {
        c(p_display, rep("", nrow(out) - 1))
      }
    }

    out
  }

  tables <- lapply(table_vars, function(var_name) {
    x <- data[[var_name]]

    if (is_categorical_var(x, var_name)) {
      categorical_table(var_name)
    } else if (is.numeric(x)) {
      numeric_table(var_name)
    } else {
      warning(
        "Treating `", var_name, "` as categorical because it is not numeric, factor, character, logical, or labelled.",
        call. = FALSE
      )
      categorical_table(var_name)
    }
  })

  out <- do.call(rbind, tables)
  row.names(out) <- NULL
  out
}

bivariate_tabler <- function(data, variable_name, by, fill_p = T) {
    variable_data = data[, c(variable_name, by)]
    valid <- sum(!is.na(variable_data[[variable_name]]))
    categorical = ifelse(is.factor(variable_data[[variable_name]]) || is.character(variable_data[[variable_name]]), T, F)
    variable_data = drop_na(variable_data)
    if(valid == 0) return(NULL)
    if(categorical){
        table <- table(variable_data)
        possible_chisq.test <- possibly(.f = chisq.test, otherwise = NULL)
        possible_fisher.test <- possibly(.f = fisher.test, otherwise = NULL)
        chisq_test <- possible_chisq.test(table, simulate.p.value = F)
        fisher_test <- possible_fisher.test(table, simulate.p.value = T)
        pvalue <- case_when(
            !is.na(chisq_test$p.value) ~ pvalue(chisq_test$p.value, accuracy = 0.001), 
            !is.na(fisher_test$p.value) ~ pvalue(fisher_test$p.value, accuracy = 0.001), 
            T ~ "Unable to calculate")
        bottom <- cbind(Total = rowSums(table, na.rm = T), as.data.frame.matrix(table))
        top <- as.data.frame.list(colSums(bottom)) %>%
            rename_with(~str_replace_all(.x, "\\.", " ")) %>%
            rename_with(~paste(.x, "N")) %>%
            mutate(Variable = variable_name, Response = "Valid N", .before = 1)
        # bottom <- rbind(`Valid N` = colSums(bottom, na.rm = T), bottom)
        names(bottom) <- paste0(names(bottom), " N")
        for(col in names(bottom)) {
            bottom[[str_replace(col, " N", " Percent")]] <- percent(bottom[[col]]/sum(bottom[[col]]), accuracy = 0.01)
            bottom <- relocate(bottom, any_of(str_replace(col, " N", " Percent")), .after = any_of(col))
        }
        bottom <- tibble::rownames_to_column(bottom, "Response")
        
        out <- bind_rows(top, bottom) %>%
            select(Response, `Total N`, `Total Percent`, order(names(.))) %>%
            mutate(Variable = variable_name, .before = 1) %>%
            mutate(p = ifelse(Response == "Valid N", pvalue, NA_real_), .after = last_col()) %>%
            mutate(across(everything(), as.character)) %>%
            mutate(across(ends_with("Percent"), ~ifelse(.x == "0.00%", "--", .x)))
    } else {
        possible_oneway.test <- possibly(.f = oneway.test, otherwise = NULL)
        test <- possible_oneway.test(as.formula(paste0(variable_name, "~", by)), data = na.omit(variable_data))
        pvalue <- ifelse(is.null(test), "Unable to calculate", pvalue(test$p.value, accuracy = 0.001))
        
        overall <- bind_rows(
            variable_data %>%
                summarise(
                    !!sym(by) := "Total",
                    Response = "Valid N",
                    N = valid),
            variable_data %>%
                group_by(!!sym(by)) %>%
                summarise(Response = "Valid N",
                    N = sum(!is.na(!!sym(variable_name))))) %>%
            pivot_wider(names_from = !!sym(by), values_from = c(N)) %>%
            mutate(across(everything(), as.character)) %>%
            rename_with(~paste0("N_", .x), .cols = !Response)
        
        mean <-
            bind_rows(
                variable_data %>%
                    summarise(
                        !!sym(by) := "Total",
                        Response = "Mean (SD)",
                        N = sprintf("%.02f", mean(!!sym(variable_name), na.rm = T)),
                        Percent  = sprintf("%.02f", sd(!!sym(variable_name), na.rm = T))),
                variable_data %>%
                    group_by(!!sym(by)) %>%
                    summarise(Response = "Mean (SD)",
                        N = sprintf("%.02f", mean(!!sym(variable_name), na.rm = T)),
                        Percent  = sprintf("%.02f", sd(!!sym(variable_name), na.rm = T)))) %>%
            pivot_wider(names_from = !!sym(by), values_from = c(N, Percent))
        
        median <- bind_rows(
            variable_data %>%
                summarise(
                    !!sym(by) := "Total",
                    Response = "Median (IQR)",
                    N = sprintf("%.02f", median(!!sym(variable_name), na.rm = T)),
                    Percent  = sprintf("%.02f", (IQR(!!sym(variable_name), na.rm = T)))),
            variable_data %>%
                group_by(!!sym(by)) %>%
                summarise(
                    Response = "Median (IQR)",
                    N = sprintf("%.02f", median(!!sym(variable_name), na.rm = T)),
                    Percent  = sprintf("%.02f", IQR(!!sym(variable_name), na.rm = T)))
            ) %>%
            pivot_wider(names_from = !!sym(by), values_from = c(N, Percent))
        
        range <- bind_rows(
            variable_data %>%
                summarise(
                    !!sym(by) := "Total",
                    Response = "Range (25th-75th)",
                    N = paste0(range(!!sym(variable_name), na.rm = T), collapse = "-"),
                    Percent  = paste0(round(quantile(!!sym(variable_name), probs = c(0.25, 0.75), na.rm = T), 1), collapse = "-")),
            variable_data %>%
                group_by(!!sym(by)) %>%
                summarise(
                    Response = "Range (25th-75th)",
                    N = paste0(range(!!sym(variable_name), na.rm = T), collapse = "-"),
                    Percent  = paste0(round(quantile(!!sym(variable_name), probs = c(0.25, 0.75), na.rm = T), 1), collapse = "-"))) %>%
                pivot_wider(names_from = !!sym(by), values_from = c(N, Percent))
        
        out <-
            bind_rows(overall, mean, median, range) %>%
            rename_with(function(name) paste(str_replace(name, "^N_", ""), "N"), .cols = starts_with("N_")) %>%
            rename_with(function(name) paste(str_replace(name, "^Percent_", ""), "Percent"), .cols = starts_with("Percent_")) %>%
            mutate(
                p = ifelse(Response == "Valid N", pvalue, NA_real_), .after = last_col(),
                across(everything(), as.character),
                Variable = variable_name
            )
            
    }
    order <- c("Overall", levels(variable_data[[by]]))
    order <- unlist(map(order, ~paste(.x, c("N", "Percent"))))

    rownames(out) <- NULL
    out[nrow(out) + 1,] <- NA
    if(fill_p) out <- fill(out, p, .direction = "down")
    
    Label <- var_label(variable_data[[variable_name]])
    out$Label <- ifelse(is.null(Label), "", Label)

    select(out, Variable, Label, Response, `Total N`, `Total Percent`, any_of(order), p)
}
