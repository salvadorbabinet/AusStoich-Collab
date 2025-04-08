find_combination_pars <- function(data_pars, data) {

    # data pars is your list of parameters that you want to itertiely include

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Initialize the best model with basic parameters
    remaining <- 1:length(data_pars)
    taken <- length(remaining) + 1 # out of the range of values such that remaining[-taken] = remaining for the first iteration

    # best model list
    best <- list(AIC = 0)
    val <- 0

    base_row <- all_pars
    base_row[names(all_pars)] <- NA
    base_row <- c(likelihood = 0, base_row)

    should_continue <- TRUE
    # Iteratively add parameters and evaluate the model. Keep only AIC improvements.
    for (i in 1:length(data_pars)) {
        if (!should_continue) break

        iter_df <- tibble()
        for (j in remaining[-taken]) {
            # print(j)
            # j = 1

            model <- run_optim(data, inipar, conditions) #your wrapper function for the lm
            iter_row <- base_row
            iter_row[names(inipar)] <- model$par
            iter_row["likelihood"] <- model$value # this will be your R squared

            iter_df <- bind_rows(iter_df, iter_row)
        }
        
        iter_df <- as.data.frame(do.call(rbind, iter_df))

        best_model <- which.min(iter_df$likelihood)
        # best_model_AIC <- 2 * iter_df$likelihood[best_model] + 2 * (i + val + 1)
        best_model_AIC <- iter_df$likelihood[best_model]
        print(best_model_AIC)
        if (best$AIC == 0 | best_model_AIC < best$AIC) { # set here if Rsquared new is > Rsquared old
            best$AIC <- best_model_AIC
            best$par <- iter_df[best_model, names(all_pars)]
            best$par <- Filter(function(x) !is.na(x), best$par)
            taken <- which(sapply(data_pars, function(x) any(grepl(x, names(best$par)))))
            print(paste0("num parameters included: ", i, "parameters taken: ", toString(data_pars[taken])))
        } else {
            not_taken <- data_pars[!data_pars %in% names(best$par)]
            print(paste("No improvement. Exiting loop. Parameters not taken:", toString(not_taken)))
            should_continue <- FALSE
        }
    }
    return(best$par)
}


#to add categorical into this, just add another conditional
#if numerical do this (what ana wrote, but modified to R2)
#if categorical : do this (what i will write)