data(mtcars)
librarian::shelf(tidyverse, broom, broom.helpers, scales, writexl)

data = mtcars %>%
    mutate(test = factor(rep(0:1, 16)),
        vs = as.factor(vs),
        gear = as.factor(vs),
        carb = as.factor(carb))
dependents = c("mpg", "cyl")
independents = c("disp", "hp", "drat", "wt", "qsec", "carb", "gear")
distributions = c("gaussian", "poisson")
main_independent = NULL
fit_statistics = T
marginal_means = c("carb", "gear")
outname = NULL

glm_multiple <- function(data, dependents, independents, main_independent = NULL, distributions, marginal_means = character(0), fit_statistics = F, num_format = 0.001, p_format = num_format, outname = NULL) {
    if(!require("librarian")){
        install.packages("librarian")
    } else {
        librarian::shelf(tidyverse, broom, broom.helpers, scales, writexl)
    }
    
    lets_try <- function(...) {quietly(safely(...))}
    
    create_tidy_model <- function(model, dist){
        pvalue_format <- if(is.null(p_format)) function(x) return(x) else pvalue_format(accuracy = p_format)
        number_format <- if(is.null(num_format)) function(x) return(x) else number_format(accuracy = num_format)
        
        tidy_model <- model %>%
            tidy_and_attach(tidy_fun = broom.helpers::tidy_parameters, exponentiate = ifelse(dist == "gaussian", F, T)) %>%
            tidy_disambiguate_terms() %>%
            tidy_identify_variables() %>%
            tidy_remove_intercept() %>%
            tidy_add_reference_rows() %>%
            tidy_add_variable_labels() %>%
            tidy_add_term_labels() %>%
            tidy_add_n() %>%
            tidy_select_variables(include = if(any(!is.na(main_independent))) all_of(main_independent) else everything()) %>%
            tidy_add_coefficients_type() %>%
            tidy_detach_model() %>%
            transmute(
                Variable = variable,
                `Variable Label` = var_label,
                `Variable Type` = str_to_sentence(var_type),
                Level = ifelse(`Variable Type` == "Continuous", NA_character_, label),
                N = n_obs,
                `Estimate Type` = attr(., "coefficients_label"),
                Estimate = estimate,
                SE = std.error,
                LCL = conf.low,
                UCL = conf.high,
                P = pvalue_format(p.value),
                across(c(Estimate, SE, LCL, UCL), number_format),
                across(c(P, Estimate, SE, LCL, UCL), ~ifelse(!is.na(reference_row) & reference_row == T & is.character(.x), "--", .x)),
            )
    }
    
    outname <- tools::file_path_sans_ext(outname)
    indep_vars <- unique(c(independents, main_independent))
    clean_indep_vars <- unique(unlist(str_split(indep_vars, pattern = regex("\\*|\\:"))))
    
    stopifnot("Distributions must be same length as dependent variables." = length(dependents) == length(distributions))
    stopifnot("Distributions must be gaussian, binomial, poisson, Gamma, or negbin." = all(distributions %in% c("gaussian", "binomial", "poisson", "negbin", "Gamma")))
    stopifnot("Must specify at least 1 independent variable." = length(clean_indep_vars) > 1)
    stopifnot("Variaables selected for margins must be in independent variables." = length(marginal_means) == 0 | all(marginal_means %in% clean_indep_vars))
    stopifnot("Variables specified for marginal means must be categorical or a factor." = !any(map_lgl(marginal_means, function(x) is.numeric(data[[x]]))))

    data_list <- map(dependents, function(dep_var) dplyr::select(data, any_of(c(dep_var, clean_indep_vars))))
    
    model_list <- imap(data_list, function(model_data, num) {
        formula <- as.formula(sprintf("%s ~ %s", names(model_data)[1], paste0(indep_vars, collapse = " + ")))
        if(distributions[[num]] == "negbin"){
            librarian::shelf(MASS)
            safe_glm.nb <- lets_try(glm.nb)
            out <- safe_glm.nb(formula = formula, data = model_data)
        } else {
            safe_glm <- lets_try(glm)
            out <- safe_glm(formula = formula, data = model_data, family = distributions[[num]])
        }
        out$warnings <- if(length(out$warnings) == 0) "None" else paste(seq_along(unique(out$warnings)), unique(out$warnings), sep = ".", collapse = "; ")
        out$messages <- if(is.null(out$messages) || length(out$messages) == 0) "None" else paste(seq_along(unique(out$warnings)), unique(out$warnings), sep = ".", collapse = "; ")
        
        list(result = out$result$result, error = out$result$error, warning = out$warnings, message = out$messages)
    }) 

    imap(model_list, function(model, num) {
        if(is.null(model$error)) {
            lets_try_create_tidy_model <- lets_try(create_tidy_model)
            out <- lets_try_create_tidy_model(model$result, dist = distributions[[num]])
            out <- list(result = out$result$result, error = out$result$error, warning = out$warnings, message = out$messages)

            if(is.null(out$error)){
                out$result %>%
                    mutate(
                        Warnings = if(length(out$warning) == 0) "None" else paste(seq_along(unique(out$warning)), unique(out$warning), sep = ".", collapse = "; "),
                        Messages = if(length(out$message) == 0) "None" else paste(seq_along(unique(out$message)), unique(out$message), sep = ".", collapse = "; ")
                    )
            }
        } else {
            tibble(Error = as.character(out$error))
        }
        }) %>%
        set_names(dependents) %>%
        write_xlsx(path = sprintf("%sRegression Results %s.xlsx", if(length(outname) == 0) "" else paste0(outname, " "), Sys.Date()))

    if(fit_statistics){
        librarian::shelf(performance)
        imap(model_list, function(model, num){
            if(is.null(model$error)) {
                model$result %>%
                    performance() %>%
                    as_tibble() %>%
                    mutate(Outcome = dependents[[num]], Type = distributions[[num]], .before = 1) %>%
                    mutate(Dispersion = sum(residuals(model$result, "pearson")^2)) %>%
                    mutate(Warning = model$warning, Message = model$message)
            } else {
                tibble(Outcome = dependents[[num]], Type = distributions[[num]], Error = as.character(model$error))
            }
            }) %>%
            bind_rows() %>%
            relocate(any_of(c("Warning", "Message", "Error")), .after = dplyr::last_col()) %>%
            write_xlsx(path = sprintf("%sRegression Fit %s.xlsx", if(length(outname) == 0) "" else paste0(outname, " "), Sys.Date()))
    }

    if(length(marginal_means) > 0) {
        librarian::shelf(emmeans)
        imap(model_list, function(model, num) {
            if(is.null(model$error)) {
                emmeans(object = model$result, specs = marginal_means) %>% 
                    pairs()
                    as_tibble()
                    transmute(
                        Mean = emmean, SE = SE,
                        LCL = lower.CL, UCL = upper.CL
                    )
            } else {model$error}
        }) %>%
            set_names(dependents) %>%
            write_xlsx(path = sprintf("%sRegression margins %s.xlsx", if(length(outname) == 0) "" else paste0(outname, " "), Sys.Date()))
    }
}

glm_multiple(
    data = data, 
    dependents = c("mpg", "cyl"),
    independents = c("disp", "hp", "drat", "wt", "qsec", "vs", "disp*wt"), 
    # main_independent = "disp:wt",
    distributions = c("gaussian", "poisson"),
    marginal_means = T,
    fit_statistics = T,
    outname = "Test"
    )
