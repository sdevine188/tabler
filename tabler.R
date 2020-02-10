library(Hmisc)
library(tidyverse)
library(tidyselect)
library(testthat)
library(attempt)
library(rlang)

# note that because loading dplyr before Hmisc will cause Hmisc's summarize() to mask dplyr's summarize()
# all calls to summarize() are specified as dplyr::summarize()



# create tabler function
tabler <- function(data = NULL, rows = NULL, cols = NULL, stats = NULL, stat_vars = NULL, 
                   quantile = c(0, .25, .5, .75, 1), 
                   na.rm = FALSE, na.rm_true_stats = NULL, na.rm_false_stats = NULL,
                   weights = NULL, normalize_weights = TRUE) {
        
        
        #######################
        
        
        # check to ensure mandatory data argument is not null
        if(is.null(data)) {
                stop("The data argument is required, but not input was given.")
        }

        # check to ensure mandatory rows argument is not null
        if(is.null(deparse(substitute(rows)))) {
                stop("The rows argument is required, but not input was given.")
        }
        
        
        ######################
        
        
        # handle row argument to determine if it's a single bare variable, or 
        # quosures, or character
        
        # will pass row to deparse(substitute()) to see if it's a single bare variable
        # if so, will overwrite row with the deparsed string
        # note this handler won't allow passing multiple bare variables in c()
        # if you want multiple bar variables, pass into vars
        if(deparse(substitute(rows)) %in% names(data)) {
                
                rows <- deparse(substitute(rows))
        } 
        
        # handle row if it's passed using quo(), quos(), or vars(), including tidyselect helpers
        # would be nice to issue custom error when tidyselectors have ONLY invalid vars
        # default is to give uninformative error message "Error in -x : invalid argument to unary operator"
        # can use attempt's try_catch, with custom if()/str_replace function to micro-handle different errors
        if("quosure" %in% class(rows) | "quosures" %in% class(rows)) {
                
                # handle bare variables passed to vars() that are not found in data
                try_catch(expr = rows_placeholder <- data %>% ungroup() %>% select(!!!rows) %>% names(), .e = function(e) {
                        
                        if(str_detect(string = as.character(e), 
                                      pattern = regex("object .* not found\n"))) {
                                
                                var_not_found <- str_extract(string = as.character(e), 
                                                             pattern = regex('object .* not found\n$')) %>% 
                                        str_replace(string = ., pattern = "object ", replacement = "") %>% 
                                        str_replace(string = ., pattern = " not found\n$", 
                                                    replacement = "")
                                
                                stop(str_glue("The following variable passed to the rows ",
                                              "argument is not found in the data: ",
                                              "{var_not_found}"))
                        } 
                })
                
                # handle cases where all tidyselect helpers are not found in data
                # when this is the case, rows is set as character(0)
                # this leads downstream to an "Error in -x : invalid argument to unary operator"
                # but easier to catch it here
                if(length(data %>% select(!!!rows) %>% names()) == 0) {
                        stop(str_glue("The following tidyselect helpers ",
                                      "passed to the rows argument do not match to any variables found in the data: ",
                                      "{str_c((map(.x = rows, .f = as_label) %>% unlist()), collapse = ', ')}"))
                }
                
                # if no errors have been raised, overwrite rows with rows_placeholder
                rows <- rows_placeholder
        }
        
        # if rows is not a valid single bare variable or a quosure, it is a non-valid bare variable
        # or a character, or something and the argument tests below will ensure 
        # that all variables in rows match names in the data, or will throw error
        
        
        #################
        
        
        # handle col argument to determine if it's a single bare variable, or 
        # quosures, or character
        
        # will pass col to deparse(substitute()) to see if it's a single bare variable
        # if so, will overwrite col with the deparsed string
        # note this handler won't allow passing multiple bare variables in c()
        # if you want multiple bar variables, pass into vars
        if(deparse(substitute(cols)) %in% names(data)) {
                cols <- deparse(substitute(cols))
        }
        
        # handle col if it's passed from quo(), quos(), or vars()
        if("quosure" %in% class(cols) | "quosures" %in% class(cols)) {
                
                # handle bare variables passed to vars() that are not found in data
                try_catch(expr = cols_placeholder <- data %>% select(!!!cols) %>% names(), .e = function(e) {
                        
                        if(str_detect(string = as.character(e), 
                                      pattern = regex("object .* not found\n"))) {
                                
                                var_not_found <- str_extract(string = as.character(e), 
                                                             pattern = regex('object .* not found\n$')) %>% 
                                        str_replace(string = ., pattern = "object ", replacement = "") %>% 
                                        str_replace(string = ., pattern = " not found\n$", 
                                                    replacement = "")
                                
                                stop(str_glue("The following variable passed to the cols ",
                                              "argument is not found in the data: ",
                                              "{var_not_found}"))
                        } 
                })
                
                # handle cases where all tidyselect helpers are not found in data
                # when this is the case, cols is set as character(0)
                # this leads downstream to an "Error in -x : invalid argument to unary operator"
                # but easier to catch it here
                if(length(data %>% select(!!!cols) %>% names()) == 0) {
                        stop(str_glue("The following tidyselect helpers ",
                                      "passed to the cols argument do not match to any variables found in the data: ",
                                      "{str_c((map(.x = cols, .f = as_label) %>% unlist()), collapse = ', ')}"))
                }
                
                # if no errors have been raised, overwrite cols with cols_placeholder
                cols <- cols_placeholder
        }
        
        # if cols is not a valid single bare variable or a quosure, it is a non-valid bare variable
        # or a character, and the argument tests below will ensure 
        # that all variables in cols match names in the data, or will throw error
        
        
        ######################
        
        
        # handle stat_vars argument to determine if it's a single bare variable, or 
        # quosures, or character
        
        # will pass stat_vars to deparse(substitute()) to see if it's a single bare variable
        # if so, will overwrite stat_vars with the deparsed string
        # note this handler won't allow passing multiple bare variables in c()
        # if you want multiple bar variables, pass into vars
        if(deparse(substitute(stat_vars)) %in% names(data)) {
                
                stat_vars <- deparse(substitute(stat_vars))
        } 
        
        # handle row if it's passed using quo(), quos(), or vars(), including tidyselect helpers
        # would be nice to issue custom error when tidyselectors have ONLY invalid vars
        # default is to give uninformative error message "Error in -x : invalid argument to unary operator"
        # can use attempt's try_catch, with custom if()/str_replace function to micro-handle different errors
        if("quosure" %in% class(stat_vars) | "quosures" %in% class(stat_vars)) {
                
                # handle bare variables passed to vars() that are not found in data
                try_catch(expr = stat_vars_placeholder <- data %>% select(!!!stat_vars) %>% names(), 
                          .e = function(e) {
                        
                        if(str_detect(string = as.character(e), 
                                      pattern = regex("object .* not found\n"))) {
                                
                                var_not_found <- str_extract(string = as.character(e), 
                                                             pattern = regex('object .* not found\n$')) %>% 
                                        str_replace(string = ., pattern = "object ", replacement = "") %>% 
                                        str_replace(string = ., pattern = " not found\n$", 
                                                    replacement = "")
                                
                                stop(str_glue("The following variable passed to the stat_vars ",
                                              "argument is not found in the data: ",
                                              "{var_not_found}"))
                        } 
                })
                
                # handle cases where all tidyselect helpers are not found in data
                # when this is the case, stat_vars is set as character(0)
                # this leads downstream to an "Error in -x : invalid argument to unary operator"
                # but easier to catch it here
                if(length(data %>% select(!!!stat_vars) %>% names()) == 0) {
                        stop(str_glue("The following tidyselect helpers ",
                                      "passed to the stat_vars argument do not match to any variables found in the data: ",
                                      "{str_c((map(.x = stat_vars, .f = as_label) %>% unlist()), collapse = ', ')}"))
                }
                
                # if no errors have been raised, overwrite stat_vars with stat_vars_placeholder
                stat_vars <- stat_vars_placeholder
        }
        
        ############################################################################################
        
        
        # check to see if stat argument was passed value(s) or should assume default values, 
        # note default stat values depend on whether any col arguments were passed
        
        # create cols_present to flag whether any col arguments were passed
        if(is.null(cols)) {
                cols_present <- FALSE
        } else {cols_present <- TRUE}
        
        # set stat to default "n" if no stat arguments were passed and cols_present = TRUE
        if(is.null(stats) & cols_present == TRUE) {
                stats <- "n"
        } 
        
        # set stat to default c("n", "pct_col") 
        # if no stat arguments passed and cols_present = FALSE
        if(is.null(stats) & cols_present == FALSE) {
                stats <- c("n", "pct_col")
        } 
        
        
        ############################################################################################
        
        
        # check arguments
        if((tibble(row_vars = rows) %>% mutate(row_in_data = row_vars %in% names(data)) %>% 
            filter(row_in_data == FALSE) %>% nrow()) > 0) {
                row_vars_not_in_data <- tibble(row_vars = rows) %>% 
                        mutate(row_in_data = row_vars %in% names(data)) %>% 
                        filter(row_in_data == FALSE) %>% pull(row_vars) %>%
                        str_c(string = ., collapse = ", ")
                stop(str_c("The following variables passed to the rows argument are not found in the data: ",
                           row_vars_not_in_data))
        }
        
        if(!is.null(cols)) {
                if((tibble(col_vars = cols) %>% mutate(col_in_data = col_vars %in% names(data)) %>% 
                    filter(col_in_data == FALSE) %>% nrow()) > 0) {
                        col_vars_not_in_data <- tibble(col_vars = cols) %>% 
                                mutate(col_in_data = col_vars %in% names(data)) %>% 
                                filter(col_in_data == FALSE) %>% pull(col_vars) %>%
                                str_c(string = ., collapse = ", ")
                        stop(str_c("The following variables passed to the cols argument are not found in the data: ",
                                   col_vars_not_in_data))
                }  
        }
        
        if(!is.null(stat_vars)) {
                if((tibble(stat_vars_arg = stat_vars) %>% 
                    mutate(stat_vars_in_data = stat_vars_arg %in% names(data)) %>% 
                    filter(stat_vars_in_data == FALSE) %>% nrow()) > 0) {
                        stat_vars_not_in_data <- tibble(stat_vars_arg = stat_vars) %>% 
                                mutate(stat_vars_in_data = stat_vars_arg %in% names(data)) %>% 
                                filter(stat_vars_in_data == FALSE) %>% pull(stat_vars_arg) %>%
                                str_c(string = ., collapse = ", ")
                        stop(str_c("The following variables passed to the stat_ars argument ", 
                                   "are not found in the data: ", stat_vars_not_in_data))
                }  
        }
        
        if(sum(stats %in% c("n", "pct_col", "pct_row", "pct_all", "valid_pct_col", "valid_pct_row",
                            "valid_pct_all", "n_distinct", "mode", "sum", "mean", "median", "sd", "min", "max", 
                            "quantile")) != length(stats)) {
                stop("The stats argument can only include one or more of the following: 
                     n', 'pct_col', 'pct_row', 'pct_all', 'valid_pct_col', 'valid_pct_row',
                        'valid_pct_all', 'n_distinct', 'mode', 'sum', 'mean', 'median', 'sd', 'min', 'max', 
                     'quantile'.  When no variables are passed to the cols argument, 
                        the default is c('n', 'pct_col'), and just 'n' otherwise.")
        }
        
        if((sum(stats %in% c("n_distinct", "mode", "sum", "mean", "median", "sd", "min", "max", 
                             "quantile")) > 0) & (is.null(stat_vars))) {
                
                stats_requiring_stat_var <- tibble(stats_arg = stats) %>% 
                        filter(stats_arg %in% c("n_distinct", "mode", "sum", "mean", "median", "sd", "min", "max", 
                                                "quantile")) %>%
                        pull(stats_arg) %>% str_c(string = ., collapse = ", ")
                
                stop(str_glue("The stats argument included {stats_requiring_stat_var}, ",
                              "so the stat_vars argument must specify at least one variable ",
                              "and cannot be NULL."))
        }
        
        if("quantile" %in% stats & is.null(quantile)) {
                stop("The string 'quantile' was passed to the stat argument, ",
                     "but the quantiles argument was set to NULL. ",
                     "If 'quantile' is passed to the stat argument, ",
                     "then the quantile argument must be left at the default values of 'c(0, .25, .5, .75, 1)', or",
                     "the desired quantiles must be passed to the quantile argument.")
        }
        
        if(sum(quantile >= 0 & quantile <= 1) != length(quantile)) {
                stop("The quantile argument was passed a value that is outside the range 0 to 1, ",
                     "but can only accept numeric values from 0 and 1. ",
                     "The default values are 'c(0, .25, .5, .75, 1).'")
        }
        
        if(class(quantile) != c("numeric")) {
                stop("The quantile argument was passed a non-numeric value, ",
                     "but can only accept numeric values from 0 and 1. ",
                     "The default values are 'c(0, .25, .5, .75, 1).'")
        }
        
        if(!normalize_weights %in% c(TRUE, FALSE)) {
                stop("The normalize_weights argument must be either TRUE or FALSE (default).")
        }
        
        if(!(is.null(na.rm_true_stats)) | !(is.null(na.rm_false_stats))) {
                
                na.rm_stats_not_in_stats_arg <- tibble(na.rm_stats = c(na.rm_true_stats, na.rm_false_stats)) %>%
                        anti_join(., tibble(stats = stats), by = c("na.rm_stats" = "stats")) %>% 
                        pull(na.rm_stats)
                
                if(length(na.rm_stats_not_in_stats_arg) > 0) {
                        stop(str_glue("The following stats were passed to the na.rm_true_stats or ",
                                        "na.rm_false_stats arguments, but were not also passed to the ",
                                      "stats argument, which is required: ",
                                      "{str_c(na.rm_stats_not_in_stats_arg, collapse = ', ')}"))
                }
        }
        
        if(!(is.null(na.rm_true_stats)) & !(is.null(na.rm_false_stats))) {
                
                stats_passed_to_both_na.rm_args <- tibble(na.rm_true_stats = na.rm_true_stats) %>%
                        inner_join(., tibble(na.rm_false_stats = na.rm_false_stats),
                                   by = c("na.rm_true_stats" = "na.rm_false_stats")) %>% 
                        pull(na.rm_true_stats)
                
                if(length(stats_passed_to_both_na.rm_args) > 0) {
                        stop(str_glue("The following stats were passed to both the na.rm_true_stats ",
                                      "argument and the na.rm_false_stats argument, which is invalid: ",
                                      "{str_c(stats_passed_to_both_na.rm_args, collapse = ', ')}"))
                }
        }
        
        if(!(is.null(na.rm_true_stats)) | !(is.null(na.rm_false_stats))) {
                
                stats_passed_but_not_taking_na.rm_arg <- tibble(na.rm_stats = c(na.rm_true_stats, na.rm_false_stats)) %>%
                        inner_join(., tibble(stats = c("n", "pct_col", "pct_row", "pct_all",
                  "valid_pct_col", "valid_pct_row", "valid_pct_all")), by = c("na.rm_stats" = "stats")) %>% 
                        pull(na.rm_stats)
                
                if(length(stats_passed_but_not_taking_na.rm_arg) > 0) {
                        stop(str_glue("The following stats were passed to the na.rm_true_stats or ",
                                      "na.rm_false_stats arguments, but are invalid since they do ",
                                      "not require an na.rm argument: ",
                                      "{str_c(stats_passed_but_not_taking_na.rm_arg, collapse = ', ')}"))
                }
        }
        
        
        ################################################################################################
        
        
        # add default weights if none are provided
        # note that if data supplied to tabler already has a variable named "weights", 
        # but no variable is passed to weights argument, 
        # the code below will throw a warning 
        # otherwise it will add "weights" variable to data using default weight of 1, 
        # or creating "weights" var by copying variable name passed to weights argument 
        if(is.null(weights)) {
                
                if("weights" %in% names(data)) {
                        warning(str_c("Data has pre-existing column called weights, which will be overwritten with default weight of 1 ",
                                      "since no variable was passed to the weights argument"))
                }
                
                current_weights = rep(1, times = nrow(data))
                data <- data %>% mutate(weights = current_weights)
        }
        
        if(!is.null(weights) & class(weights) == "character") {
                
                if("weights" %in% names(data) & weights != "weights") {
                        warning(str_c("Data has pre-existing column called weights", 
                                      "which will be overwritten with values from the '", weights, 
                                      "' variable passed to the weights argument"))
                }
                
                weights_var_sym <- sym(weights)
                data <- data %>% mutate(weights = !!weights_var_sym)
        }
        
        if(!is.null(weights) & class(weights) != "character") {
                stop("Weights argument must be a string containing variable name for weights")
        }
        
        ################################################################################################
        
        
        # convert arguments to syms
        
        # get rows_syms
        rows_syms <- syms(rows)
        
        # get cols_syms
        cols_syms <- syms(cols)
        
        # get stat_vars_syms
        stat_vars_syms <- syms(stat_vars)
        
        # get stat_vars_numeric
        # for some reason get a "Deprecated" warning, not sure why, but suppressing it
        stat_vars_numeric <- suppressWarnings(data %>% select(!!!stat_vars_syms) %>% 
                                                      select_if(.predicate = function(col) { class(col) %in% c("numeric", "integer", "logical") } )) %>% 
                names()
        
        # get stat_vars_numeric_syms
        stat_vars_numeric_syms <- syms(stat_vars_numeric)
        
        
        #################################################################################################
        
        
        # get row_index_tbl
        row_index_tbl <- data %>% group_by(!!!rows_syms) %>% count() %>% select(-n) %>% ungroup() %>% 
                arrange(!!!rows_syms)
        
        
        ################################################################################################
        
        
        # create n_output_table
        # note that for simplicity, n_output_table is always created, because it is used elsewhere below
        
        # create_output_vars function to get final_output_col_vars when cols_present = TRUE
        # also used to get row/col_var_tbl for attributes
        create_output_vars <- function(current_var_name, current_var_number, n_output_table_initial) {
                
                # get current__var_name_sym
                current_var_name_sym <- sym(current_var_name)
                
                # add current_ to n_output_table
                current_n_output_table <- n_output_table_initial %>% mutate(current_var = current_var_name)
                
                # unite current_ and its values
                current_n_output_table <- current_n_output_table %>% 
                        unite(col = output_var, current_var, !!current_var_name_sym, sep = ".", remove = FALSE)
                
                # add number to output_var and rename output_var in current_n_output_table
                current_output_var <- str_c("output_var_", current_var_number)
                current_n_output_table <- current_n_output_table %>% 
                        rename(!!sym(current_output_var) := output_var)
                
                # select output_
                current_output_var <- current_n_output_table %>% 
                        select(!!!rows_syms, !!!cols_syms, !!sym(current_output_var))
                return(current_output_var)
        }
        
        # with cols_present = FALSE
        if(cols_present == FALSE) {
                
                # if normalize_weights = TRUE, get n_output_table
                if(normalize_weights == TRUE) {
                        n_output_table <- data %>% group_by(!!!rows_syms) %>% count() %>% ungroup() 
                }
                
                if(normalize_weights == FALSE) {
                        n_output_table <- data %>% group_by(!!!rows_syms) %>% count(wt = weights) %>% ungroup() 
                }
                
                # create n_output_table_initial that is just a copy of n_output_table
                # since this will be needed to get row_var_tbl for attributes
                n_output_table_initial <- n_output_table
        }
        
        # with cols_present = TRUE
        if(cols_present == TRUE) {
                
                # create n_output_table_initial (long form)
                n_output_table_initial <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>% 
                        count(wt = weights) %>% ungroup() 
                
                # get output_col_vars (this format is needed to bind_rows with data later on)
                output_col_vars <- map2_dfc(.x = cols, .y = 1:length(cols),
                                           .f = ~ create_output_vars(current_var_name = .x, 
                                                                     current_var_number = .y,
                                    n_output_table_initial = n_output_table_initial)) %>% 
                        select(!!!rows_syms, !!!cols_syms, starts_with("output_var_")) %>%
                        rename_at(.vars = vars(contains("output_var")), 
                                  .funs = ~ str_replace(string = ., pattern = "output_var", 
                                                        replacement = "output_col")) %>%
                        unite(col = row_index_key, !!!rows_syms, !!!cols_syms, sep = "_", remove = FALSE)
           
                # get row_index_key_tbl
                row_index_key_tbl <- output_col_vars %>%
                        mutate(row_index_tbl_row_number = row_number())
                
                # get n_output_table
                n_output_table <- n_output_table_initial %>%
                        unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                        left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)),
                                  by = "row_index_key") %>%
                        arrange(row_index_tbl_row_number) %>%
                        select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                        mutate(col_vars = str_c(col_vars, "__n")) %>%
                        spread(key = col_vars, value = n)
        }
        
        # test that n_output_table accounts for every observation somewhere 
        # test_that("test n_output_table", {
        #         
        #         expect_equal(n_output_table %>% select_if(.p = is.numeric) %>% 
        #                              map(.x = ., .f = ~ sum(.x, na.rm = TRUE)) %>% 
        #                              enframe() %>% unnest() %>% summarize(sum = sum(value)) %>%
        #                              pull(sum),
        #                      expected = data %>% summarize(weights_sum = sum(weights, na.rm = TRUE)))
        # })
        
        
        #################################################################################################
        #################################################################################################
        
        
        # use row_index_key_tbl to create row_var_tbl and col_var_tbl, which are saved as attributes
        if(cols_present == TRUE) {
                
                # create col_var_tbl
                col_var_tbl <- map2_dfc(.x = cols, .y = 1:length(cols), 
                                        .f = ~ create_output_vars(current_var_name = .x, 
                                                                  current_var_number = .y,
                                n_output_table_initial = n_output_table_initial)) %>% 
                        select(!!!cols_syms, starts_with("output_var_")) %>%
                        rename_at(.vars = vars(contains("output_var")),
                                  .funs = ~ str_replace(string = ., pattern = "output_var",
                                                        replacement = "output_col")) %>%
                        unite(col = combined_output_col_vars, starts_with("output_col"), 
                              sep = "_x_", remove = FALSE) %>% 
                        select(!!!cols_syms, starts_with("output_col_"), 
                               combined_output_col_vars)
        } else {
                col_var_tbl <- NA
        }
        
        
        ###################
        
        
        # create row_var_tbl
        row_var_tbl <- map2_dfc(.x = rows, .y = 1:length(rows),
                                   .f = ~ create_output_vars(current_var_name = .x, 
                                                             current_var_number = .y, 
                                                n_output_table_initial = n_output_table_initial)) %>%
                select(!!!rows_syms, starts_with("output_var_")) %>%
                rename_at(.vars = vars(contains("output_var")), 
                          .funs = ~ str_replace(string = ., 
                                                pattern = "output_var", replacement = "output_row")) %>%
                unite(col = final_output_row_vars, starts_with("output_row"), sep = "_x_", remove = FALSE) %>%
                select(!!!rows_syms, starts_with("output_row_"), everything())
        

        ################################################################################################
        ################################################################################################
        
        
        # get pct_all_output_table
        
        # with cols_present = FALSE
        if("pct_all" %in% stats & cols_present == FALSE) {
                
                # get pct_all_output_table
                pct_all_output_table <- n_output_table %>% 
                        mutate(sum_n = sum(n, na.rm = TRUE), pct_all = n / sum_n) %>%
                        select(-c(n, sum_n))
        }
        
        # with cols_present = TRUE
        if("pct_all" %in% stats & cols_present == TRUE) {
                
                # get sum_n across all columns
                sum_n <- n_output_table %>% select(-c(!!!rows_syms)) %>% 
                        map(.x = ., .f = ~ sum(.x, na.rm = TRUE)) %>% enframe() %>% unnest(cols = "value") %>%
                        dplyr::summarize(sum_n = sum(value, na.rm = TRUE)) %>% pull(sum_n)
                
                # get percentages across all columns
                pct_all_output_table <- n_output_table %>% select(-c(!!!rows_syms)) %>% 
                        map_dfr(.x = ., .f = ~ .x / sum_n) %>% 
                        bind_cols(n_output_table %>% select(!!!rows_syms), .)
                
                # add stat to variable names
                names(pct_all_output_table) <- str_replace(string = names(pct_all_output_table),
                                                       pattern = regex("__n$"), 
                                                       replacement = "__pct_all")
        }
        
        
        ####################################################################################
        
        
        # get pct_row_output_table
        
        # with cols_present = TRUE
        if("pct_row" %in% stats & cols_present == FALSE) {
                
                # get pct_row_output_table
                pct_row_output_table <- n_output_table %>% mutate(pct_row = n / n) %>% select(-n)
        }
        
        # with cols_present = FALSE
        if("pct_row" %in% stats & cols_present == TRUE ) {
                
                # get col variables from n_output_table that are now united
                non_row <- tibble(var_names = names(n_output_table)) %>% 
                        filter(!(var_names %in% rows)) %>% pull(var_names)
                non_rows_syms <- syms(non_row)
                
                # get pct_row_output_table
                pct_row_output_table <- n_output_table %>% select(!!!non_rows_syms) %>%
                        rowSums(na.rm = TRUE) %>% tibble(row_sum = .) %>%
                        bind_cols(n_output_table, .) %>%
                        gather(key = variable, value = value, -c(!!!rows_syms, row_sum)) %>%
                        mutate(value_pct = value / row_sum) %>% select(!!!rows_syms, variable, value_pct) %>%
                        spread(key = variable, value = value_pct)
                
                # add stat to variable names
                names(pct_row_output_table) <- str_replace(string = names(pct_row_output_table),
                                                       pattern = regex("__n$"),
                                                       replacement = "__pct_row")
        }
        
        
        ######################################################################################3
        
        
        # get pct_col_output_table
        
        # with cols_present = FALSE
        if("pct_col" %in% stats & cols_present == FALSE) {
                
                # get pct_col_output_table
                pct_col_output_table <- n_output_table %>% mutate(pct_col = n / sum(n)) %>% select(-n)
        }
        
        # with cols_present = TRUE
        if("pct_col" %in% stats & cols_present == TRUE) {
                
                # get col var names that are now united
                non_row <- tibble(var_names = names(n_output_table)) %>% 
                        filter(!(var_names %in% rows)) %>% pull(var_names)
                non_rows_syms <- syms(non_row)
                
                # get sum of all col vars
                col_sums_table <- n_output_table %>% select(!!!non_rows_syms) %>% 
                        colSums(na.rm = TRUE)
                col_sums_table_vars <- names(col_sums_table)
                
                # create get_col_sums_tbl function
                get_col_sums_tbl <- function(.x, .y) {
                        
                        # get current value and var name/sym
                        current_col_sums_table_value <- .x
                        current_col_sums_table_var <- .y
                        current_col_sums_table_var_sym <- sym(current_col_sums_table_var)
                        
                        # create tbl
                        tibble(!!current_col_sums_table_var_sym := current_col_sums_table_value) %>%
                                gather(key = variable, value = sum_n)
                }
                
                # get col_sums_tbl to join with n_output_table for calculating pct_col
                col_sums_tbl <- map2_dfr(.x = col_sums_table, .y = col_sums_table_vars, .f = get_col_sums_tbl)
                
                # get pct_col_output_table
                pct_col_output_table <- n_output_table %>% 
                        gather(key = variable, value = n, -c(!!!rows_syms)) %>%
                        left_join(., col_sums_tbl, by = "variable") %>% mutate(pct_col = n / sum_n) %>%
                        select(!!!rows_syms, variable, pct_col) %>%
                        spread(key = variable, value = pct_col)
                
                # add stat to variable names
                names(pct_col_output_table) <- str_replace(string = names(pct_col_output_table),
                                                       pattern = regex("__n$"), 
                                                       replacement = "__pct_col")
        }
        
        
        ####################################################################################
        ######################################################################################
        
        
        # get valid_pct_col/row/all_output_table
        
        # get valid_pct_output_table_row, regardless of row/col/all type
        if("valid_pct_row" %in% stats | "valid_pct_col" %in% stats | "valid_pct_all" %in% stats) {
                
                # get valid_pct_output_table row
                valid_pct_output_table_row <- map(.x = rows, 
                                                  .f = ~ row_index_tbl %>% mutate(row_number = row_number()) %>% 
                                                          select(row_number, !!sym(.x)) %>% filter(!is.na(!!sym(.x)))) %>% 
                        reduce(.x = ., .f = ~ inner_join(.x, .y, by = "row_number"))
        }
        
        
        ################
        
        
        # get finalize_valid_pct_output_table_wo_cols function, 
        # used when cols_present = FALSE, regardless of row/col/all type
        finalize_valid_pct_output_table_wo_cols <- function(valid_pct_output_table, pct_type) {
                
                # add back the NA row
                valid_pct_suffix <- str_c("valid_pct_", pct_type)
                valid_pct_output_table <- n_output_table %>% mutate(row_number = row_number()) %>% 
                        anti_join(., valid_pct_output_table_row, by = "row_number") %>%
                        map_at(.x = ., 
                               .at = n_output_table %>% select(-c(!!!rows_syms)) %>% names(), .f = ~ NA) %>%
                        as_tibble() %>% select(-row_number) %>% rename(!!sym(valid_pct_suffix) := n) %>%
                        bind_rows(valid_pct_output_table, .) %>% select(-row_number)
                
                # reorder row of valid_pct_output_table based on n_output_table
                valid_pct_output_table <- valid_pct_output_table %>%  
                        unite(col = united_row_index, !!!rows_syms, remove = FALSE) %>%
                        left_join(n_output_table %>% unite(col = united_row_index, !!!rows_syms) %>% 
                                          select(united_row_index), ., by = "united_row_index") %>%
                        select(-united_row_index)
                
                return(valid_pct_output_table)
        }
        
        
        ################
        
        
        # if cols_present = FALSE
        if(("valid_pct_row" %in% stats | "valid_pct_col" %in% stats | "valid_pct_all" %in% stats) & 
           cols_present == FALSE) {
                
                # get n_output_table_for_valid_pct
                n_output_table_for_valid_pct <- n_output_table %>% mutate(row_number = row_number()) %>%
                        select(-c(!!!rows_syms)) %>%
                        left_join(valid_pct_output_table_row, ., by = "row_number")
                
                # get valid_pct_all_output_table
                if("valid_pct_all" %in% stats) {
                        
                        valid_pct_all_output_table <- n_output_table_for_valid_pct %>% 
                                mutate(sum_n = sum(n, na.rm = TRUE), valid_pct_all = n / sum_n) %>%
                                select(-c(n, sum_n)) %>% 
                                finalize_valid_pct_output_table_wo_cols(pct_type = "all")
                } 
                
                # get valid_pct_row_output_table
                if("valid_pct_row" %in% stats) {
                        
                        valid_pct_row_output_table <- n_output_table_for_valid_pct %>% 
                                mutate(valid_pct_row = n / n) %>% select(-n) %>% 
                                finalize_valid_pct_output_table_wo_cols(pct_type = "row")
                } 
                
                # get valid_pct_col_output_table
                if("valid_pct_col" %in% stats) {
                        
                        valid_pct_col_output_table <- n_output_table_for_valid_pct %>% 
                                mutate(valid_pct_col = n / sum(n)) %>% select(-n) %>% 
                                finalize_valid_pct_output_table_wo_cols(pct_type = "col")
                }
        }
        
        
        #################
        
        
        # create get_na_output_col_vars function, used when cols_present = TRUE, regardless if row/col/all type
        get_na_output_col_vars <- function(.x, current_stat) {
                
                # get current col
                current_col <- .x
                
                # get current_col_var_name_sym
                current_col_var_name_sym <- sym(current_col)
                
                # get current_col_na_values
                current_col_na_output_table <- n_output_table_initial %>%
                        mutate(row_number = row_number()) %>%
                        filter(is.na(!!current_col_var_name_sym))
                
                # add current_col to current_col_na_values
                current_col_na_output_table <- current_col_na_output_table %>%
                        mutate(current_col = current_col)
                
                # unite current_col and its values
                current_col_na_output_table <- current_col_na_output_table %>%
                        unite(col = output_col, current_col, !!current_col_var_name_sym, sep = ".")
                
                # select output_col
                output_col <- current_col_na_output_table %>% select(row_number, output_col)
                return(output_col)
        }
        
        
        #################
        
        
        # get valid_pct_output_col_vars, for use calculating valid_pct when cols_present = TRUE,
        # regardless of whether pct_type = all/row/col
        if(("valid_pct_row" %in% stats | "valid_pct_col" %in% stats | "valid_pct_all" %in% stats) & 
           cols_present == TRUE) {
                
                # call get_na_output_col_vars
                na_output_col_vars <- map(.x = cols,
                                          .f = ~ get_na_output_col_vars(.x, current_stat = "n")) %>%
                        enframe() %>% unnest(cols = value) %>% select(row_number)
                
                # get na_output_col_vars which has final na_output variable name after combining with other groups
                na_output_col_vars <- output_col_vars %>% mutate(row_number = row_number()) %>%
                        left_join(na_output_col_vars, ., by = "row_number") %>%
                        select(-row_number) %>%
                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                        mutate(col_vars = str_c(col_vars, "__n")) %>% pull(col_vars)
                
                # if there are no na_output_col_vars, then just return original n_output_table names
                if(length(na_output_col_vars) == 0) {
                        valid_pct_output_col_vars <- n_output_table %>% names()
                }
                
                # if there are na output_col_vars, get valid_pct_output_table_col by dropping na col
                if(length(na_output_col_vars) != 0) {
                        valid_pct_output_col_vars <- n_output_table %>%
                                select(-c(!!!syms(na_output_col_vars))) %>% names()
                }
        }
        
        
        ########################
        
        
        # get finalize_valid_pct_output_table_w_cols, 
        # used when cols_present = TRUE, regardless if row/col/all type
        finalize_valid_pct_output_table_w_cols <- function(valid_pct_output_table, pct_type) {
                
                # add back the NA col, ensuring all values are set to NA since they shouldn't have valid_pct
                valid_pct_output_table <- n_output_table %>% select(!!!syms(na_output_col_vars)) %>%
                        map_dfr(.x = ., .f = ~ ifelse(!is.na(.x), NA, .x)) %>%
                        mutate(row_number = row_number()) %>%
                        left_join(valid_pct_output_table_row, ., by = "row_number") %>%
                        select(-c(row_number, !!!rows_syms)) %>%
                        bind_cols(valid_pct_output_table)
                
                # add back the NA row
                valid_pct_output_table <- n_output_table %>% mutate(row_number = row_number()) %>%
                        anti_join(., valid_pct_output_table_row, by = "row_number") %>%
                        map_at(.x = .,
                               .at = n_output_table %>% select(-c(!!!rows_syms)) %>% names(), .f = ~ NA) %>%
                        as_tibble() %>% select(-row_number) %>%
                        bind_rows(valid_pct_output_table, .)
                
                # reorder row and col of valid_pct_output_table based on n_output_table
                valid_pct_output_table <- valid_pct_output_table %>% select(names(n_output_table)) %>%
                        unite(col = united_row_index, !!!rows_syms, remove = FALSE) %>%
                        left_join(n_output_table %>% unite(col = united_row_index, !!!rows_syms) %>%
                                          select(united_row_index), ., by = "united_row_index") %>%
                        select(-united_row_index)
                
                # add stat to variable names
                valid_pct_suffix <- str_c("__valid_pct_", pct_type)
                names(valid_pct_output_table) <- valid_pct_output_table %>%
                        select(-c(!!!rows_syms)) %>% names() %>%
                        str_replace(string = ., pattern = regex("__n$"), replacement = valid_pct_suffix) %>%
                        c(rows, .)
                
                return(valid_pct_output_table)
        }
        
        
        ##########################
        
        
        # if cols_present = TRUE and pct_type = "all"
        if(("valid_pct_row" %in% stats | "valid_pct_col" %in% stats | "valid_pct_all" %in% stats) & 
           cols_present == TRUE) {
                
                # get n_output_table_for_valid_pct
                n_output_table_for_valid_pct <- n_output_table %>% mutate(row_number = row_number()) %>%
                        select(-c(!!!rows_syms)) %>%
                        left_join(valid_pct_output_table_row, ., by = "row_number") %>%
                        select(!!!syms(valid_pct_output_col_vars))
                
                if("valid_pct_all" %in% stats) {
                        
                        # get sum_n across all columns
                        sum_n <- n_output_table_for_valid_pct %>%
                                select(-c(!!!rows_syms)) %>%
                                map(.x = ., .f = ~ sum(.x, na.rm = TRUE)) %>% enframe() %>% 
                                unnest(cols = value) %>%
                                dplyr::summarize(sum_n = sum(value, na.rm = TRUE)) %>% pull(sum_n)
                        
                        # get_valid_pct_all_output_table
                        valid_pct_all_output_table <- n_output_table_for_valid_pct %>% 
                                select(-c(!!!rows_syms)) %>%
                                map_dfr(.x = ., .f = ~ .x / sum_n) %>%
                                bind_cols(n_output_table_for_valid_pct %>% select(!!!rows_syms), .) %>%
                                finalize_valid_pct_output_table_w_cols(pct_type = "all")
                        
                } 
                
                if("valid_pct_row" %in% stats) {
                        
                        # get col variables from n_output_table that are now united
                        non_row <- tibble(var_names = names(n_output_table_for_valid_pct)) %>% 
                                filter(!(var_names %in% rows)) %>% pull(var_names)
                        non_rows_syms <- syms(non_row)
                        
                        # get valid_pct_row_output_table
                        valid_pct_row_output_table <- n_output_table_for_valid_pct %>% select(!!!non_rows_syms) %>%
                                rowSums(na.rm = TRUE) %>% tibble(row_sum = .) %>%
                                bind_cols(n_output_table_for_valid_pct, .) %>%
                                gather(key = variable, value = value, -c(!!!rows_syms, row_sum)) %>%
                                mutate(value_pct = value / row_sum) %>% select(!!!rows_syms, variable, value_pct) %>%
                                spread(key = variable, value = value_pct) %>%
                                finalize_valid_pct_output_table_w_cols(pct_type = "row")
                        
                } 
                
                if("valid_pct_col" %in% stats) {
                        
                        # get col var names that are now united
                        non_row <- tibble(var_names = names(n_output_table_for_valid_pct)) %>% 
                                filter(!(var_names %in% rows)) %>% pull(var_names)
                        non_rows_syms <- syms(non_row)
                        
                        # get sum of all col vars
                        col_sums_table <- n_output_table_for_valid_pct %>% select(!!!non_rows_syms) %>% 
                                colSums(na.rm = TRUE)
                        col_sums_table_vars <- names(col_sums_table)
                        
                        # create get_col_sums_tbl function
                        get_col_sums_tbl <- function(.x, .y) {
                                
                                # get current value and var name/sym
                                current_col_sums_table_value <- .x
                                current_col_sums_table_var <- .y
                                current_col_sums_table_var_sym <- sym(current_col_sums_table_var)
                                
                                # create tbl
                                tibble(!!current_col_sums_table_var_sym := current_col_sums_table_value) %>%
                                        gather(key = variable, value = sum_n)
                        }
                        
                        # get col_sums_tbl to join with n_output_table for calculating pct
                        col_sums_tbl <- map2_dfr(.x = col_sums_table, .y = col_sums_table_vars, 
                                                 .f = get_col_sums_tbl)
                        
                        # get pct_output_table
                        valid_pct_col_output_table <- n_output_table_for_valid_pct %>% 
                                gather(key = variable, value = n, -c(!!!rows_syms)) %>%
                                left_join(., col_sums_tbl, by = "variable") %>% mutate(pct = n / sum_n) %>%
                                select(!!!rows_syms, variable, pct) %>%
                                spread(key = variable, value = pct) %>% 
                                finalize_valid_pct_output_table_w_cols(pct_type = "col")
                }
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_n_distinct function
        loop_through_stat_vars_get_n_distinct <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- sym(current_stat_var)
                
                # get n_distinct_na.rm
                if("n_distinct" %in% na.rm_true_stats) {
                        n_distinct_na.rm <- TRUE
                } else if("n_distinct" %in% na.rm_false_stats) {
                        n_distinct_na.rm <- FALSE
                } else {
                        n_distinct_na.rm <- na.rm
                }
                
                # get n_distinct_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get n_distinct_output_table
                        n_distinct_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                distinct(!!current_stat_var_sym) %>%
                                dplyr::summarize(n = n_distinct(!!current_stat_var_sym, 
                                                               na.rm = n_distinct_na.rm)) %>% ungroup()

                        # rename stat
                        names(n_distinct_output_table) <- c(rows, str_c("n_distinct.", current_stat_var))
                }
                
                # get n_distinct_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get n_distinct_output_table
                        n_distinct_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>% 
                                distinct(!!current_stat_var_sym) %>%
                                dplyr::summarize(n = n_distinct(!!current_stat_var_sym, 
                                                               na.rm = n_distinct_na.rm)) %>% ungroup() %>% 
                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                          by = "row_index_key") %>%
                                arrange(row_index_tbl_row_number) %>% 
                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__n_distinct", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = n) 
                }
                
                return(n_distinct_output_table)
        }
        
        # create n_distinct_output_table 
        if("n_distinct" %in% stats) {
                
                # call loop_through_stat_vars_get_mean
                n_distinct_output_table <- map(.x = stat_vars, .f = loop_through_stat_vars_get_n_distinct) %>%
                        reduce(left_join, by = rows)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_mode function
        loop_through_stat_vars_get_mode <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- sym(current_stat_var)
                
                # get mode_na.rm
                if("mode" %in% na.rm_true_stats) {
                        mode_na.rm <- TRUE
                } else if("mode" %in% na.rm_false_stats) {
                        mode_na.rm <- FALSE
                } else {
                        mode_na.rm <- na.rm
                }
                
                # get mode_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get mode_output_table, if normalize_weights = TRUE
                        if(normalize_weights == TRUE) {
                                
                                # note that ties for most frequent are broken 
                                # by taking first alphabetical/lowest value of current_stat_var
                                
                                if(mode_na.rm == TRUE) {
                                        
                                        mode_output_table <- data %>% group_by(!!!rows_syms) %>%
                                                count(!!current_stat_var_sym) %>%
                                                filter(!(is.na(!!current_stat_var_sym))) %>%
                                                group_by(!!!rows_syms) %>%
                                                arrange(desc(n), !!current_stat_var_sym) %>% 
                                                slice(1) %>%
                                                ungroup() %>% select(-n) %>% 
                                                rename(mode := !!current_stat_var)
                                }
                                
                                if(mode_na.rm == FALSE) {
                                        
                                        mode_output_table <- data %>% group_by(!!!rows_syms) %>%
                                                count(!!current_stat_var_sym) %>%
                                                group_by(!!!rows_syms) %>%
                                                arrange(desc(n), !!current_stat_var_sym) %>% 
                                                slice(1) %>%
                                                ungroup() %>% select(-n) %>% 
                                                rename(mode := !!current_stat_var)
                                }
                        }
                        
                        # get mode_output_table, if normalize_weights = FALSE
                        # need to multiply observation counts by weights
                        if(normalize_weights == FALSE) {
                                
                                # note that ties for most frequent are broken 
                                # by taking first alphabetical/lowest value of current_stat_var
                                
                                if(mode_na.rm == TRUE) {
                                        
                                        mode_output_table <- data %>% group_by(!!!rows_syms, weights) %>%
                                                count(!!current_stat_var_sym) %>%
                                                ungroup() %>%
                                                mutate(n = n * weights) %>%
                                                # need to sum to account for different weight values
                                                # leading to multiple row per row/stat_var combos
                                                group_by(!!!rows_syms, !!current_stat_var_sym) %>%
                                                dplyr::summarize(n = sum(n, na.rm = TRUE)) %>% ungroup() %>%
                                                group_by(!!!rows_syms) %>%
                                                arrange(desc(n), !!current_stat_var_sym) %>% 
                                                filter(!(is.na(!!current_stat_var_sym))) %>%
                                                slice(1) %>%
                                                ungroup() %>% select(-n) %>% 
                                                rename(mode := !!current_stat_var)
                                }

                                if(mode_na.rm == FALSE) {
                                        
                                        mode_output_table <- data %>% group_by(!!!rows_syms, weights) %>%
                                                count(!!current_stat_var_sym) %>%
                                                ungroup() %>%
                                                mutate(n = n * weights) %>%
                                                # need to sum to account for different weight values
                                                # leading to multiple row per row/stat_var combos
                                                group_by(!!!rows_syms, !!current_stat_var_sym) %>%
                                                dplyr::summarize(n = sum(n, na.rm = TRUE)) %>% ungroup() %>%
                                                group_by(!!!rows_syms) %>%
                                                arrange(desc(n), !!current_stat_var_sym) %>% 
                                                slice(1) %>%
                                                ungroup() %>% select(-n) %>% 
                                                rename(mode := !!current_stat_var)
                                }
                        }
                        
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("mode.", current_stat_var)
                        current_stat_variable_name_sym <- sym(current_stat_variable_name)
                        
                        # rename current_stat_variable_name
                        mode_output_table <- mode_output_table %>%
                                rename(!!current_stat_variable_name_sym := "mode") 
                        
                        return(mode_output_table)
                }
                
                
                # get mode_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get mode_output_table, if normalize_weights = TRUE
                        if(normalize_weights == TRUE) {
                                
                                # note that ties for most frequent are broken 
                                # by taking first alphabetical/lowest value of current_stat_var
                                if(mode_na.rm == TRUE) {
                                        
                                        mode_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>%
                                                count(!!current_stat_var_sym) %>% 
                                                arrange(desc(n), !!current_stat_var_sym) %>% 
                                                filter(!(is.na(!!current_stat_var_sym))) %>% slice(1) %>%
                                                ungroup() %>% 
                                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                                          by = "row_index_key") %>%
                                                arrange(row_index_tbl_row_number) %>% 
                                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                                rename(mode := !!current_stat_var_sym) %>%
                                                mutate(col_vars = str_c(col_vars, "__mode", ".", current_stat_var)) %>%
                                                select(-n) %>%
                                                spread(key = col_vars, value = mode)
                                }
                                 
                                if(mode_na.rm == FALSE) {
                                        
                                        mode_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>%
                                                count(!!current_stat_var_sym) %>% 
                                                arrange(desc(n), !!current_stat_var_sym) %>% slice(1) %>%
                                                ungroup() %>% 
                                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                                          by = "row_index_key") %>%
                                                arrange(row_index_tbl_row_number) %>% 
                                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                                rename(mode := !!current_stat_var_sym) %>%
                                                mutate(col_vars = str_c(col_vars, "__mode", ".", current_stat_var)) %>%
                                                select(-n) %>%
                                                spread(key = col_vars, value = mode)
                                }
                        }
                        
                        # get mode_output_table, if normalize_weights = FALSE
                        # need to multiply observation counts by weights
                        if(normalize_weights == FALSE) {
                                
                                # note that ties for most frequent are broken 
                                # by taking first alphabetical/lowest value of current_stat_var
                                if(mode_na.rm == TRUE) {
                                        
                                        mode_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms, weights) %>%
                                                count(!!current_stat_var_sym) %>% 
                                                mutate(n = n * weights) %>%
                                                ungroup() %>% 
                                                # need to sum to account for different weight values
                                                # leading to multiple row per row/stat_var combos
                                                group_by(!!!rows_syms, !!!cols_syms, !!current_stat_var_sym) %>%
                                                dplyr::summarize(n = sum(n, na.rm = TRUE)) %>% ungroup() %>%
                                                group_by(!!!rows_syms, !!!cols_syms) %>%
                                                arrange(desc(n), !!current_stat_var_sym) %>% 
                                                filter(!(is.na(!!current_stat_var_sym))) %>% slice(1) %>%
                                                ungroup() %>% select(-n) %>%
                                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)),
                                                          by = "row_index_key") %>%
                                                arrange(row_index_tbl_row_number) %>%
                                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                                rename(mode := !!current_stat_var_sym) %>%
                                                mutate(col_vars = str_c(col_vars, "__mode", ".", current_stat_var)) %>%
                                                spread(key = col_vars, value = mode)
                                }
                
                                if(mode_na.rm == FALSE) {
                                        
                                        mode_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms, weights) %>%
                                                count(!!current_stat_var_sym) %>% 
                                                mutate(n = n * weights) %>%
                                                ungroup() %>% 
                                                # need to sum to account for different weight values
                                                # leading to multiple row per row/stat_var combos
                                                group_by(!!!rows_syms, !!!cols_syms, !!current_stat_var_sym) %>%
                                                dplyr::summarize(n = sum(n, na.rm = TRUE)) %>% ungroup() %>%
                                                group_by(!!!rows_syms, !!!cols_syms) %>%
                                                arrange(desc(n), !!current_stat_var_sym) %>% slice(1) %>%
                                                ungroup() %>% select(-n) %>%
                                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)),
                                                          by = "row_index_key") %>%
                                                arrange(row_index_tbl_row_number) %>%
                                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                                rename(mode := !!current_stat_var_sym) %>%
                                                mutate(col_vars = str_c(col_vars, "__mode", ".", current_stat_var)) %>%
                                                spread(key = col_vars, value = mode)
                                }
                        }
                        
                        return(mode_output_table)
                }
        }
        
        # create mode_output_table with cols_present = FALSE
        if("mode" %in% stats) {
                
                # call loop_through_stat_vars_get_mode
                mode_output_table <- map(.x = stat_vars, .f = loop_through_stat_vars_get_mode) %>%
                        reduce(left_join, by = rows)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_sum function
        loop_through_stat_vars_get_sum <- function(.x) {

                # get current_stat_var
                current_stat_var <- .x

                # get current_stat_var_sym
                current_stat_var_sym <- sym(current_stat_var)
                
                # get sum_na.rm
                if("sum" %in% na.rm_true_stats) {
                        sum_na.rm <- TRUE
                } else if("sum" %in% na.rm_false_stats) {
                        sum_na.rm <- FALSE
                } else {
                        sum_na.rm <- na.rm
                }

                # get sum_output_table if cols_present = FALSE
                if(cols_present == FALSE) {

                        # get sum_output_table
                        sum_output_table <- data %>% group_by(!!!rows_syms) %>%
                                dplyr::summarize(sum = sum(!!current_stat_var_sym * weights, 
                                                           na.rm = sum_na.rm)) %>%
                                ungroup()


                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("sum.", current_stat_var)
                        current_stat_variable_name_sym <- sym(current_stat_variable_name)

                        # rename current_stat_variable_name
                        sum_output_table <- sum_output_table %>%
                                rename(!!current_stat_variable_name_sym := "sum")

                        return(sum_output_table)
                }


                # get sum_output_table if cols_present = TRUE
                if(cols_present == TRUE) {

                        # get sum_output_table
                        sum_output_table <- data %>%
                                group_by(!!!rows_syms, !!!cols_syms) %>%
                                dplyr::summarize(sum = sum(!!current_stat_var_sym * weights,
                                                           na.rm = sum_na.rm)) %>%
                                ungroup() %>%
                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)),
                                          by = "row_index_key") %>%
                                arrange(row_index_tbl_row_number) %>%
                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__sum", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = sum)

                        return(sum_output_table)
                }
        }

        # create sum_output_table
        if("sum" %in% stats) {

                # call loop_through_stat_vars_get_sum
                sum_output_table <- map(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_sum) %>%
                        reduce(left_join, by = rows)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_mean function
        loop_through_stat_vars_get_mean <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- sym(current_stat_var)
                
                # get mean_na.rm
                if("mean" %in% na.rm_true_stats) {
                        mean_na.rm <- TRUE
                } else if("mean" %in% na.rm_false_stats) {
                        mean_na.rm <- FALSE
                } else {
                        mean_na.rm <- na.rm
                }
                
                # get mean_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get mean_output_table
                        mean_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                dplyr::summarize(mean = wtd.mean(!!current_stat_var_sym, weights = weights, 
                                                                 normwt = normalize_weights, 
                                                                 na.rm = mean_na.rm)) %>% 
                                ungroup()
                        
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("mean.", current_stat_var)
                        current_stat_variable_name_sym <- sym(current_stat_variable_name)
                        
                        # rename current_stat_variable_name
                        mean_output_table <- mean_output_table %>%
                                rename(!!current_stat_variable_name_sym := "mean") 
                        
                        return(mean_output_table)
                }
                
                
                # get mean_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get mean_output_table
                        mean_output_table <- data %>% 
                                group_by(!!!rows_syms, !!!cols_syms) %>% 
                                dplyr::summarize(mean = wtd.mean(!!current_stat_var_sym, weights = weights, 
                                                                 normwt = normalize_weights, 
                                                                 na.rm = mean_na.rm)) %>% 
                                ungroup() %>%
                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                          by = "row_index_key") %>%
                                arrange(row_index_tbl_row_number) %>% 
                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__mean", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = mean) 
                        
                        return(mean_output_table)
                }
        }
        
        # create mean_output_table 
        if("mean" %in% stats) {
                
                # call loop_through_stat_vars_get_mean
                mean_output_table <- map(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_mean) %>%
                        reduce(left_join, by = rows)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_median function
        loop_through_stat_vars_get_median <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- sym(current_stat_var)
                
                # get median_na.rm
                if("median" %in% na.rm_true_stats) {
                        median_na.rm <- TRUE
                } else if("median" %in% na.rm_false_stats) {
                        median_na.rm <- FALSE
                } else {
                        median_na.rm <- na.rm
                }
                
                # get median_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get median_output_table
                        median_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                dplyr::summarize(median = list(enframe(wtd.quantile(!!current_stat_var_sym, 
                                                                                    probs = .5, weights = weights, 
                                                                                    normwt = normalize_weights, 
                                                                                    na.rm = median_na.rm)))) %>% 
                                unnest(cols = median) %>% ungroup() %>% 
                                mutate(name = str_c("median", ".", current_stat_var)) %>%
                                spread(key = name, value = value)

                        return(median_output_table)
                }
                
                
                # get median_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get median_output_table
                        median_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>% 
                                dplyr::summarize(median = list(enframe(wtd.quantile(!!current_stat_var_sym, 
                                                                                    probs = .5, weights = weights, 
                                                                                    normwt = normalize_weights, 
                                                                                    na.rm = median_na.rm)))) %>% 
                                unnest(cols = median) %>% ungroup() %>% 
                                mutate(name = "median") %>%
                                spread(key = name, value = value) %>% 
                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                          by = "row_index_key") %>%
                                arrange(row_index_tbl_row_number) %>% 
                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__median", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = median) 
                        
                        return(median_output_table)
                }
        }
        
        # create median_output_table 
        if("median" %in% stats) {
                
                # call loop_through_stat_vars_get_median
                median_output_table <- map(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_median) %>%
                        reduce(left_join, by = rows)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_sd function
        loop_through_stat_vars_get_sd <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- sym(current_stat_var)
                
                # get sd_na.rm
                if("sd" %in% na.rm_true_stats) {
                        sd_na.rm <- TRUE
                } else if("sd" %in% na.rm_false_stats) {
                        sd_na.rm <- FALSE
                } else {
                        sd_na.rm <- na.rm
                }
                
                # get sd_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get sd_output_table
                        sd_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                dplyr::summarize(sd = suppressWarnings(sqrt(wtd.var(!!current_stat_var_sym, 
                                                                                    weights = weights, 
                                                                                    normwt = normalize_weights, 
                                                                                    na.rm = sd_na.rm)))) %>% 
                                ungroup()
                        
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("sd.", current_stat_var)
                        current_stat_variable_name_sym <- sym(current_stat_variable_name)
                        
                        # rename current_stat_variable_name
                        sd_output_table <- sd_output_table %>%
                                rename(!!current_stat_variable_name_sym := "sd") 
                        
                        return(sd_output_table)
                }
                
                
                # get sd_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get sd_output_table
                        sd_output_table <- data %>% 
                                group_by(!!!rows_syms, !!!cols_syms) %>% 
                                dplyr::summarize(sd = suppressWarnings(sqrt(wtd.var(!!current_stat_var_sym, 
                                                                                    weights = weights, 
                                                                                    normwt = normalize_weights, 
                                                                                    na.rm = sd_na.rm)))) %>% 
                                ungroup() %>%
                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                          by = "row_index_key") %>%
                                arrange(row_index_tbl_row_number) %>% 
                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__sd", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = sd) 
                        
                        return(sd_output_table)
                }
        }
        
        # create sd_output_table with cols_present = FALSE
        if("sd" %in% stats) {
                
                # call loop_through_stat_vars_get_sd
                sd_output_table <- map(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_sd) %>%
                        reduce(left_join, by = rows)
                        
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_min function
        loop_through_stat_vars_get_min <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- sym(current_stat_var)
                
                # get min_na.rm
                if("min" %in% na.rm_true_stats) {
                        min_na.rm <- TRUE
                } else if("min" %in% na.rm_false_stats) {
                        min_na.rm <- FALSE
                } else {
                        min_na.rm <- na.rm
                }
                
                # get min_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get min_output_table
                        min_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                dplyr::summarize(min = min(!!current_stat_var_sym, na.rm = min_na.rm)) %>% 
                                ungroup()
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("min.", current_stat_var)
                        current_stat_variable_name_sym <- sym(current_stat_variable_name)
                        
                        # rename current_stat_variable_name
                        min_output_table <- min_output_table %>%
                                rename(!!current_stat_variable_name_sym := "min") 
                        
                        return(min_output_table)
                }
                
                
                # get min_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get min_output_table
                        min_output_table <- data %>% 
                                group_by(!!!rows_syms, !!!cols_syms) %>% 
                                dplyr::summarize(min = min(!!current_stat_var_sym, na.rm = min_na.rm)) %>% 
                                ungroup() %>%
                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                          by = "row_index_key") %>%
                                arrange(row_index_tbl_row_number) %>% 
                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__min", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = min) 
                        
                        return(min_output_table)
                }
        }
        
        # create min_output_table with cols_present = FALSE
        if("min" %in% stats) {
                
                # call loop_through_stat_vars_get_min
                min_output_table <- map(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_min) %>%
                        reduce(left_join, by = rows)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_max function
        loop_through_stat_vars_get_max <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- sym(current_stat_var)
                
                # get max_na.rm
                if("max" %in% na.rm_true_stats) {
                        max_na.rm <- TRUE
                } else if("max" %in% na.rm_false_stats) {
                        max_na.rm <- FALSE
                } else {
                        max_na.rm <- na.rm
                }
                
                # get max_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get max_output_table
                        max_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                dplyr::summarize(max = max(!!current_stat_var_sym, na.rm = max_na.rm)) %>% 
                                ungroup()
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("max.", current_stat_var)
                        current_stat_variable_name_sym <- sym(current_stat_variable_name)
                        
                        # rename current_stat_variable_name
                        max_output_table <- max_output_table %>%
                                rename(!!current_stat_variable_name_sym := "max") 
                        
                        return(max_output_table)
                }
                
                
                # get max_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get max_output_table
                        max_output_table <- data %>% 
                                group_by(!!!rows_syms, !!!cols_syms) %>% 
                                dplyr::summarize(max = max(!!current_stat_var_sym, na.rm = max_na.rm)) %>% 
                                ungroup() %>%
                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                          by = "row_index_key") %>%
                                arrange(row_index_tbl_row_number) %>% 
                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__max", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = max) 
                        
                        return(max_output_table)
                }
        }
        
        # create max_output_table with cols_present = FALSE
        if("max" %in% stats) {
                
                # call loop_through_stat_vars_get_max
                max_output_table <- map(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_max) %>%
                        reduce(left_join, by = rows)
        }
        
        
        ###############################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_quantile function
        loop_through_stat_vars_get_quantile <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- sym(current_stat_var)
                
                # get quantile_na.rm
                if("quantile" %in% na.rm_true_stats) {
                        quantile_na.rm <- TRUE
                } else if("quantile" %in% na.rm_false_stats) {
                        quantile_na.rm <- FALSE
                } else {
                        quantile_na.rm <- na.rm
                }
                
                # get quantile_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get quantile_order_syms, since by default q100 is place before q25
                        quantile_order_syms <- syms(str_c("q", round(quantile, digits = 2) * 100))
                        
                        # get quantile_output_table
                        quantile_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                dplyr::summarize(quantile = list(enframe(wtd.quantile(!!current_stat_var_sym, 
                                                                                       probs = quantile, weights = weights, 
                                                                                       normwt = normalize_weights, 
                                                                                      na.rm = quantile_na.rm)))) %>% 
                                unnest(cols = quantile) %>% ungroup() %>% 
                                mutate(name = str_squish(name),
                                       name = str_replace(string = name, pattern = "%", replacement = ""),
                                       name = round(as.numeric(name), digits = 0),
                                       name = as.character(name),
                                       name = str_c("q", str_squish(name))) %>%
                                spread(key = name, value = value)
                        
                        # get quantile variable syms in order, to sort numerically
                        # eg q100 should not precede q25, q50, and q75 due to default alphanumeric sorting
                        quantile_order_syms <- syms(str_c("q", (round(quantile, digits = 2) * 100), ".", 
                                                                current_stat_var))
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("quantile.", current_stat_var)
                        current_stat_variable_name_sym <- sym(current_stat_variable_name)
                        
                        # rename current_stat_variable_name, and reorder quantiles numerically
                        quantile_output_table <- quantile_output_table %>%
                                rename_at(.vars = vars(-c(!!!rows_syms)), 
                                          .funs = ~ str_c(., ".", current_stat_var)) %>%
                                select(!!!rows_syms, !!!quantile_order_syms)
                        
                        return(quantile_output_table)
                }
                
                
                # get quantile_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get collapsed_output_col_vars
                        collapsed_output_col_vars <- output_col_vars %>% 
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>% 
                                distinct(col_vars) %>% arrange(col_vars) %>% pull(col_vars)
                        
                        # get quantile_order, since by default q100 is place before q25
                        quantile_order <- syms(str_c("q", round(quantile, digits = 2) * 100, ".", current_stat_var))
                        
                        # create get_quantile_ordered_vars function
                        get_quantile_ordered_vars <- function(.x) {
                                
                                # get current_collapsed_output_col_var
                                current_collapsed_output_col_var <- .x
                                
                                # loop through and join current_collapsed_output_col_var with quantile_order 
                                current_quantile_ordered_var <- map(.x = quantile_order, 
                                                                     .f = ~ str_c(current_collapsed_output_col_var, "__", .x)) %>%
                                        enframe() %>% unnest(cols = value) %>% select(value)
                                
                                return(current_quantile_ordered_var)
                        }
                        
                        # call get_quantile_ordered_vars
                        quantile_ordered_vars <- map_dfr(.x = collapsed_output_col_vars, 
                                                          .f = get_quantile_ordered_vars) %>%
                                pull(value)
                        
                        # get quantile_ordered_vars_syms
                        quantile_ordered_vars_syms <- syms(quantile_ordered_vars)
                        
                        # get quantile_output_table
                        quantile_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>% 
                                dplyr::summarize(quantile = list(enframe(wtd.quantile(!!current_stat_var_sym, 
                                                                                       probs = quantile, weights = weights, 
                                                                                       normwt = normalize_weights, 
                                                                                      na.rm = quantile_na.rm)))) %>% 
                                unnest(cols = quantile) %>% ungroup() %>% 
                                mutate(name = str_squish(name),
                                       name = str_replace(string = name, pattern = "%", replacement = ""),
                                       name = round(as.numeric(name), digits = 0),
                                       name = as.character(name),
                                       name = str_c("q", str_squish(name))) %>%
                                spread(key = name, value = value) %>% 
                                unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                          by = "row_index_key") %>%
                                arrange(row_index_tbl_row_number) %>% 
                                select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__")) %>%
                                gather(key = quantile, value = value, -c(!!!rows_syms, col_vars)) %>%
                                unite(col = col_vars, col_vars, quantile, sep = "") %>%
                                mutate(col_vars = str_c(col_vars, ".", current_stat_var, sep = "")) %>%
                                spread(key = col_vars, value = value) 

                        return(quantile_output_table)
                }
        }
        
        # create quantile_output_table 
        if("quantile" %in% stats) {
                
                # call loop_through_stat_vars_get_quantile
                quantile_output_table <- map(.x = stat_vars_numeric, 
                                              .f = loop_through_stat_vars_get_quantile) %>%
                        reduce(left_join, by = rows)
        }
        
        
        ###################################################################################
        ###################################################################################
        ###################################################################################
        
        
        # create final output_table when cols_present = FALSE
        if(cols_present == FALSE) {
                
                # create loop_through_stat_wo_col function
                loop_through_stat_wo_col <- function(.x) {
                        
                        # get current_stat
                        current_stat <- .x
                        
                        if(current_stat == "n") {
                                return(n_output_table)
                        }
                        if(current_stat == "pct_col") {
                                return(pct_col_output_table)
                        }
                        if(current_stat == "pct_row") {
                                return(pct_row_output_table)
                        }
                        if(current_stat == "pct_all") {
                                return(pct_all_output_table)
                        }
                        if(current_stat == "valid_pct_col") {
                                return(valid_pct_col_output_table)
                        }
                        if(current_stat == "valid_pct_row") {
                                return(valid_pct_row_output_table)
                        }
                        if(current_stat == "valid_pct_all") {
                                return(valid_pct_all_output_table)
                        }
                        if(current_stat == "n_distinct") {
                                return(n_distinct_output_table)
                        }
                        if(current_stat == "mean") {
                                return(mean_output_table)
                        }
                        if(current_stat == "median") {
                                return(median_output_table)
                        }
                        if(current_stat == "mode") {
                                return(mode_output_table)
                        }
                        if(current_stat == "sd") {
                                return(sd_output_table)
                        }
                        if(current_stat == "min") {
                                return(min_output_table)
                        }
                        if(current_stat == "max") {
                                return(max_output_table)
                        }
                        if(current_stat == "quantile") {
                                return(quantile_output_table)
                        }
                }
                
                # call loop_through_stat_wo_col
                output_table <- map(.x = stats, .f = loop_through_stat_wo_col) %>%
                        compact() %>% reduce(.x = ., .f = left_join, by = rows)
        }
        
        
        
        ##################################################################################
        
        
        # create final output_table, when cols_present = TRUE
        
        if(cols_present == TRUE) {
                
                # get collapsed_output_col_vars
                collapsed_output_col_vars <- output_col_vars %>% 
                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>% 
                        distinct(col_vars) %>% arrange(col_vars) %>% pull(col_vars)
                
                # create loop_through_col_vars function
                loop_through_col_vars <- function(.x) {
                        
                        # get current_output_col_var
                        current_output_col_var <- .x
                        
                        # call loop_through_stat function
                        return(map(.x = stats, 
                                .f = ~ loop_through_stat(current_output_col_var = current_output_col_var, .x)) %>%
                                       reduce(left_join, by = rows))
                }
                
                # create loop_through_stat function
                loop_through_stat <- function(current_output_col_var, .x) {
                        
                        # get current_stat
                        current_stat <- .x
                        
                        # if current_stat = n or pct (which cannot have stat_vars),
                        # then just fetch and return current_output_col_var from n/pct/valid_pct_output_table
                        if(current_stat %in% c("n", "pct_col", "pct_row", "pct_all",
                                               "valid_pct_col", "valid_pct_row", "valid_pct_all")) {
                                
                                # modify current_output_col_var to includ current_stat
                                current_output_col_var <- str_c(current_output_col_var, "__", current_stat)
                                
                                # get current_output_col_var_sym
                                current_output_col_var_sym <- sym(current_output_col_var)
                                
                                # get current_output_col_var from current_stat output_table
                                if(current_stat == "n") {
                                        current_output <- n_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "pct_col") {
                                        current_output <- pct_col_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)                
                                }
                                if(current_stat == "pct_row") {
                                        current_output <- pct_row_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)                
                                }
                                if(current_stat == "pct_all") {
                                        current_output <- pct_all_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)                
                                }
                                if(current_stat == "valid_pct_col") {
                                        current_output <- valid_pct_col_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)                
                                }
                                if(current_stat == "valid_pct_row") {
                                        current_output <- valid_pct_row_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)                
                                }
                                if(current_stat == "valid_pct_all") {
                                        current_output <- valid_pct_all_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)                
                                }
                        }
                        
                        # if current_stat != n, pct, or valid_pct, then need to loop through stat_vars
                        return(map(.x = stat_vars, 
                                .f = ~ loop_through_stat_vars(current_output_col_var = current_output_col_var,
                                                              current_stat = current_stat, .x)) %>%
                                       compact() %>% reduce(left_join, by = rows))
                }
                
                # create loop_through_stat_vars function
                loop_through_stat_vars <- function(current_output_col_var, current_stat, .x) {
                        
                        # get current_stat_var
                        current_stat_var <- .x
                        
                        # modify current_output_col_var to include current_stat_var, 
                        # unless current_stat = "quantile", which needs special handling due to naming (eg q25)
                        if(current_stat != "quantile") {
                                current_output_col_var <- str_c(current_output_col_var, "__", current_stat,
                                                                ".", current_stat_var)
                        }
                        
                        # get current_output_col_var_sym - not used to pull quantile variables though
                        current_output_col_var_sym <- sym(current_output_col_var)
                        
                        # handle quantile variables
                        if(current_stat == "quantile") {
                                current_output_col_vars <- tibble(current_output_col_var = current_output_col_var,
                                                                  quantile = str_c("q", round(quantile, digits = 2) * 100, ".", current_stat_var)) %>%
                                        mutate(current_output_col_var = str_c(current_output_col_var,
                                                                              quantile, sep = "__")) %>%
                                        pull(current_output_col_var)
                                
                                # get current_output_col_vars_syms - used to pull quantile variables
                                current_output_col_vars_syms <- syms(current_output_col_vars)
                        }
                        
                        # get current_output_col_var from current_stat output_table 
                        # for any stat_vars, regardless of whether it's character or numeric
                        if(current_stat == "n_distinct") {
                                current_output <- n_distinct_output_table %>% 
                                        select(!!!rows_syms, !!current_output_col_var_sym)
                                return(current_output)
                        }
                        
                        if(current_stat == "mode") {
                                current_output <- mode_output_table %>% 
                                        select(!!!rows_syms, !!current_output_col_var_sym)
                                return(current_output)
                        }
                        
                        # get current_output_col_var from current_stat output_table 
                        # only for cases where stat_var is numeric
                        # since current_output_col_var_sym does not exist for these stat when stat_var is character
                        # and this will throw error "<output_col_var> not found"
                        if(current_stat_var %in% stat_vars_numeric) {
                                
                                if(current_stat == "sum") {
                                        current_output <- sum_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "mean") {
                                        current_output <- mean_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "median") {
                                        current_output <- median_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)
                                }
                                
                                if(current_stat == "sd") {
                                        current_output <- sd_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "min") {
                                        current_output <- min_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "max") {
                                        current_output <- max_output_table %>% 
                                                select(!!!rows_syms, !!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "quantile") {
                                        current_output <- quantile_output_table %>% 
                                                select(!!!rows_syms, !!!current_output_col_vars_syms)
                                        return(current_output)
                                }
                        }
                }
                
                # create output_table, minus the row_index_tbl which will be added later below
                output_table <- map(.x = collapsed_output_col_vars, .f = loop_through_col_vars)  %>%
                        reduce(left_join, by = rows)
        }        
        
        
        #################################################################################################
        #################################################################################################
        
        
        # add attributes to facilitate tabler helper functions
        
        # create get_dummy_flags function
        get_dummy_flags <- function(current_var_values, current_var_name, type = NULL) {
                
                distinct_current_var_values <- tibble(current_var_values) %>% 
                        distinct() %>% arrange() %>% pull()
                
                # handle rows
                if(type == "rows") {
                        if(identical(distinct_current_var_values, c(0, 1)) | 
                           identical(distinct_current_var_values, c("0", "1"))) {
                                return(tibble(rows = current_var_name, dummy_flag = 1))
                        }
                        if(!(identical(distinct_current_var_values, c(0, 1))) & 
                           !(identical(distinct_current_var_values, c("0", "1")))) {
                                return(tibble(rows = current_var_name, dummy_flag = 0))
                        }
                }
                
                # handle cols
                if(type == "cols") {
                        if(identical(distinct_current_var_values, c(0, 1)) | 
                           identical(distinct_current_var_values, c("0", "1"))) {
                                return(tibble(cols = current_var_name, dummy_flag = 1))
                        }
                        if(!(identical(distinct_current_var_values, c(0, 1))) & 
                           !(identical(distinct_current_var_values, c("0", "1")))) {
                                return(tibble(cols = current_var_name, dummy_flag = 0))
                        }
                }
                
        }
        
        # add tabler_rows as attribute, listing rows and dummy flags
        tabler_rows <- data %>% select(!!!rows_syms) %>% 
                map2_dfr(.x = ., .y = names(.), 
                        .f = ~ get_dummy_flags(current_var_values = .x, current_var_name = .y, type = "rows"))
        attr(x = output_table, which = "tabler_rows") <- tabler_rows
        
        # add tabler_cols as attribute, listing cols and dummy flags
        tabler_cols <- data %>% select(!!!cols_syms) %>% 
                map2_dfr(.x = ., .y = names(.), 
                        .f = ~ get_dummy_flags(current_var_values = .x, current_var_name = .y, type = "cols"))
        attr(x = output_table, which = "tabler_cols") <- tabler_cols
        
        # add row_var_tbl as attribute
        attr(x = output_table, which = "tabler_row_var_tbl") <- row_var_tbl
        
        # if cols_present = TRUE, update col_var_tbl to include stats and stat_vars and add as attribute
        if(cols_present == TRUE) {
                
                col_var_tbl <- output_table %>% select(-c(!!!rows_syms)) %>% names() %>% 
                        tibble(final_output_col_vars = .) %>%
                        mutate(stats = str_extract(string = final_output_col_vars, pattern = regex("__(?!.*__).*$")),
                               stat_vars = str_replace(string = stats, pattern = regex("__.*\\."), replacement = ""),
                               stats = str_replace(string = stats, pattern = "__", replacement = ""),
                               stats = str_replace(string = stats, pattern = regex("\\..*"), replacement = ""),
                               stat_vars = case_when(stat_vars %in% c("__n", "__pct", "__valid_pct") ~ NA_character_,
                                                     TRUE ~ stat_vars),
                               combined_output_col_vars = str_extract(string = final_output_col_vars, pattern = regex("^.*__")),
                               combined_output_col_vars = str_replace(string = combined_output_col_vars, 
                                                                      pattern = "__", replacement = "")) %>%
                        left_join(., col_var_tbl, by = "combined_output_col_vars") %>%
                        select(!!!cols_syms, starts_with("output_col_"), combined_output_col_vars,
                               stats, stat_vars, final_output_col_vars)
                
                # add tabler_col_var_tbl attribute
                attr(x = output_table, which = "tabler_col_var_tbl") <- col_var_tbl
        }
        
        # if cols_present = FALSE, add NA as tabler_col_var_tbl attribute
        if(cols_present == FALSE) {
                attr(x = output_table, which = "tabler_col_var_tbl") <- NA
        }
        
        
        #################################################################################################
        #################################################################################################
        
        
        # return output table
        return(output_table)
}

