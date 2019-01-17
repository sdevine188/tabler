# need to detach dplyr, since Hmisc summarize function masks it if weights package is loaded after dplyr
# after detaching dplyr, reload tidyverse or dplyr so that instead dplyr's summarize masks Hmisc
detach_package <- function(pkg, character.only = FALSE)
{
        if(!character.only)
        {
                pkg <- deparse(substitute(pkg))
        }
        search_item <- paste("package", pkg, sep = ":")
        while(search_item %in% search())
        {
                detach(search_item, unload = TRUE, character.only = TRUE)
        }
}
detach("package:dplyr", unload=TRUE)


#########################################################################
#########################################################################
#########################################################################


library(Hmisc)
library(dplyr)
library(tidyverse)
library(gapminder)
library(testthat)

# https://www.rdocumentation.org/packages/Hmisc/versions/4.1-1/topics/wtd.stats
# https://dplyr.tidyverse.org/articles/programming.html
# http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
# http://pcwww.liv.ac.uk/~william/R/crosstab.r
# https://www.rdocumentation.org/packages/descr/versions/1.1.4/topics/CrossTable
# https://dabblingwithdata.wordpress.com/2017/12/20/my-favourite-r-package-for-frequency-tables/

# create data
# age <- c(10, 20, 30, 10, 20, 30, 30, 30, 30, 20)
# sleep <- c(8, 8, 8, 6, 5, 5, 9, 6, 5, 8)
# country <- c("England", "U.S.", "Germany", "England", "U.S.", "Germany", "U.S.", "U.S.", "U.S.", "England")
# health <- c("high", "medium", "low", "low", "low", "medium", "high", "high", "low", "medium")
# sex <- c("male", "male", "female", "female", "male", "male", "male", "male", "female", "female")
# weights <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
# data <- tibble(age, sleep, country, health, sex, weights)
# data

data_original <- gapminder %>% mutate(life_exp = case_when(lifeExp < 30 ~ "short", 
                                                  lifeExp >= 30 & lifeExp < 50 ~ "average", 
                                                  lifeExp >= 50 ~ "long"),
                             size = case_when(pop < 5000000 ~ "small", 
                                              pop >= 5000000 & pop < 100000000 ~ "medium",
                                              pop >= 100000000 ~ "large"),
                             era = case_when(year < 1950 ~ "early", 
                                             year >= 1950 & year < 1980 ~ "recent",
                                             year >= 1980 ~ "modern"),
                             country = as.character(country),
                             continent = as.character(continent)) %>% 
        filter(country != "Cote d'Ivoire") %>%
        mutate(row_number = row_number(), 
               continent = case_when(row_number == 1 ~ NA_character_, TRUE ~ continent),
               size = case_when(row_number == 2 ~ NA_character_, TRUE ~ size))
data_original


#################################################################################################3


# create tabler function
tabler <- function(data, rows, cols = NULL, weights = NULL, stats = c("n", "pct"), stat_vars = NULL, 
                   pct_type = "all", 
                   n_quantiles = NULL, quantiles = c(0, .25, .5, .75, 1), add_stat_total_row = NULL, 
                   add_stat_total_cols = NULL, stat_total_across_groups = FALSE,
                    row_name_type = "uncollapsed", col_name_type = "var.value.stat.stat_var",
                   normalize_weights = FALSE) {
        
        # check arguments
        if(missing(data)) {
                stop("The data argument is required, but no input was given.")
        }
        
        if(missing(rows)) {
                stop("The rows argument is required, but no input was given.")
        }
        
        if(sum(stats %in% c("n", "pct", "n_distinct", "mean", "median", "mode", "sd", "min", "max", 
                            "quantiles")) != length(stats)) {
                stop("The stats argument can only include one or more of the following: 
                     n', 'pct', 'n_distinct', 'mean', 'median', 'mode', 'sd', 'min', 'max', 
                     'quantiles'.  Default is c('n', 'pct').")
        }
        
        if(is.null(stats)) {
                
                stats <- "n"
        }
        
        if(!pct_type %in% c("all", "row", "col")) {
                stop("The pct_type argument must be either 'all' (default), 'row', or 'col'.")
        }
        
        if("quantiles" %in% stats & is.null(quantiles)) {
                stop("The string 'quantiles' was passed to the stats argument, ",
                "but the quantiles argument was set to NULL. ",
                "If 'quantiles' is passed to the stats argument, ",
                "then the quantiles argument must be left at the default values of 'c(0, .25, .5, .75, 1)', or",
                "the desired quantiles must be passed to the quantiles argument.")
        }
        
        if(sum(quantiles >= 0 & quantiles <= 1) != length(quantiles)) {
                stop("The quantiles argument was passed a value that is outside the range 0 to 1, ",
                     "but can only accept numeric values from 0 and 1. ",
                     "The default values are 'c(0, .25, .5, .75, 1).'")
        }
        
        if(class(quantiles) != c("numeric")) {
                stop("The quantiles argument was passed a non-numeric value, ",
                        "but can only accept numeric values from 0 and 1. ",
                     "The default values are 'c(0, .25, .5, .75, 1).'")
        }
        
        if("quantiles" %in% stats & !is.null(quantiles) & !identical(x = quantiles, y = c(0, .25, .5, .75, 1)) & 
           !is.null(n_quantiles)) {
                warning("Since values have been passed to both the quantiles argument and the n_quantiles argument, ",
                        "the quantiles argument will be ignored, and the ",
                        "n_quantiles argument will be used to calculate the desired quantiles.")
                
                # set value of quantiles using n_quantiles argument
                quantiles <- (100 / n_quantiles) * c(seq(from = 0, to = n_quantiles, by = 1))
        }
        
        if("quantiles" %in% stats & !is.null(n_quantiles)) {
                
                # set value of quantiles using n_quantiles argument
                quantiles <- (1 / n_quantiles) * c(seq(from = 0, to = n_quantiles, by = 1))
        }
        
        if(class(n_quantiles) %in% c("numeric", "integer")) {
                stop("The n_quantiles argument was passed a non-numeric and non-integer value, ",
                        "but can only accept numeric or integer values from 1 to 100.")
        }
        
        if(!is.null(n_quantiles)) {
                if(n_quantiles > 0 & n_quantiles <= 100) {
                        stop("The n_quantiles argument was passed a value that is outside the range 1 to 100, ",
                             "but can only accept numeric values from 1 and 100.")
                }
        }
        
        if(sum(add_stat_total_row %in% c("n", "pct", "n_distinct", "mean", "median", "mode", "sd", "min", "max", 
                            "quantiles")) != length(add_stat_total_row)) {
                stop("The add_stat_total_row argument can only include one or more of the following: 
                     n', 'pct', 'n_distinct', 'mean', 'median', 'mode', 'sd', 'min', 'max', 
                     'quantiles'.  Default is NULL.")
        }
        
        if(tibble(stats = add_stat_total_row) %>% anti_join(., tibble(stats), by = "stats") %>% nrow() > 0) {
                unsupported_stats <- tibble(stats = add_stat_total_row) %>% 
                        anti_join(., tibble(stats), by = "stats") %>% pull(stats) %>% str_c(., collapse = ", ")
                stop(str_c("The add_stat_total_row argument was passed values that were not passed 
                           to the stats argument: ", unsupported_stats, 
                           ". All values passed to the add_stat_total_row argument must also be passed to the
                           stats argument."))
        }
        
        if(sum(add_stat_total_cols %in% c("n", "pct", "n_distinct", "mean", "median", "mode", "sd", "min", "max", 
                                         "quantiles")) != length(add_stat_total_cols)) {
                stop("The add_stat_total_cols argument can only include one or more of the following: 
                     n', 'pct', 'n_distinct', 'mean', 'median', 'mode', 'sd', 'min', 'max', 
                     'quantiles'.  Default is NULL.")
        }
        
        if(tibble(stats = add_stat_total_cols) %>% anti_join(., tibble(stats), by = "stats") %>% nrow() > 0) {
                unsupported_stats <- tibble(stats = add_stat_total_cols) %>% 
                        anti_join(., tibble(stats), by = "stats") %>% pull(stats) %>% str_c(., collapse = ", ")
                stop(str_c("The add_stat_total_cols argument was passed values that were not passed 
                           to the stats argument: ", unsupported_stats, 
                           ". All values passed to the add_stat_total_cols argument must also be passed to the
                           stats argument."))
        }
        
        if(!col_name_type %in% c("var.value.stat.stat_var", "value.stat.stat_var", "var.value", "value")) {
                stop("The col_name_type argument must be either 'var.value.stat.stat_var' (default), 
                     'var.value', 'value.stat.stat_var', or 'value'.")
        }
        
        if(!row_name_type %in% c("uncollapsed", "var.value", "value")) {
                stop("The row_name_type argument must be either 'uncollapsed' (default), 'var.value', or 'value'.")
        }
        
        if(!normalize_weights %in% c(TRUE, FALSE)) {
                stop("The normalize_weights argument must be either TRUE or FALSE (default).")
        }
        
        
        ################################################################################################
        
        
        # add default weights if none are provided
        # note that if data supplied to tabler already has a variable named "weights", but no variable is passed to weights argument, 
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
        row_index_tbl <- data %>% group_by(!!!rows_syms) %>% count() %>% select(-n) %>% ungroup()
        
        # get col_index_tbl 
        col_index_tbl <- data %>% group_by(!!!cols_syms) %>% count() %>% select(-n) %>% ungroup()
        
   
        ################################################################################################
        
        
        # create n_output_table
        # note n_output_table is always created, because output_col_vars is used regardless of stats
        # to create final output_table
        
        # get whether cols are present
        if(is.null(cols)) {
                cols_present <- FALSE
        } else {cols_present <- TRUE}
        
        
        # with cols_present = FALSE
        if(cols_present == FALSE) {
                
                # get n_output_table
                n_output_table <- data %>% group_by(!!!rows_syms) %>% count(wt = weights) %>% ungroup()
        }
        
        # with cols_present = TRUE
        if(cols_present == TRUE) {
                
                # create n_output_table_initial (long form)
                n_output_table_initial <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>% 
                        count(wt = weights) %>% ungroup() 
                
                # create_output_col_vars function to get final col_vars
                create_output_col_vars <- function(.x, col_name_type, current_stat) {

                        # get current col
                        current_col <- .x
                        
                        # get current_col_var_name_sym
                        current_col_var_name_sym <- ensym(current_col)
                        
                        # add current_col to n_output_table
                        current_n_output_table <- n_output_table_initial %>% mutate(current_col = current_col)
                        
                        # unite current_col and its values
                        current_n_output_table <- current_n_output_table %>% 
                                unite(col = output_col, current_col, !!current_col_var_name_sym, sep = ".")
                        
                        # select output_col
                        output_col <- current_n_output_table %>% select(output_col)
                        return(output_col)
                }
                
                # get output_col_vars (this format is needed to bind_row with data later on)
                output_col_vars <- map_dfc(.x = cols, 
                        .f = ~create_output_col_vars(.x, col_name_type = col_name_type, current_stat = "n"))
                
                # get final_output_col_vars, which collapses the output_cols to how they'll look in final output
                final_output_col_vars <- output_col_vars %>% 
                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                        distinct(col_vars)
                
                # get n_output_table
                n_output_table <- n_output_table_initial %>% select(-c(!!!cols_syms)) %>% bind_cols(., output_col_vars) %>%
                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                        mutate(col_vars = str_c(col_vars, "__n")) %>%
                        spread(key = col_vars, value = n)
        }
        
        # test n_output_table
        test_that("test n_output_table", {
                
                expect_equal(n_output_table %>% select_if(.p = is.numeric) %>% 
                                     map(.x = ., .f = ~ sum(.x, na.rm = TRUE)) %>% 
                                     enframe() %>% unnest() %>% summarize(sum = sum(value)) %>%
                                     pull(sum),
                             expected = data %>% nrow())
        })
       
        
        ################################################################################################
        ################################################################################################
        
        
        # get pct_output_table
        
        # with cols_present = FALSE and pct_type = all
        if("pct" %in% stats & cols_present == FALSE & pct_type == "all") {
                
                # get pct_output_table
                pct_output_table <- n_output_table %>% mutate(sum_n = sum(n, na.rm = TRUE), pct = n / sum_n) %>%
                        select(-c(n, sum_n))
        }
        
        # with cols_present = TRUE and pct_type = all
        if("pct" %in% stats & cols_present == TRUE & pct_type == "all") {
                
                # get sum_n across all columns
                sum_n <- n_output_table %>% select(-c(!!!rows_syms)) %>% 
                        map(.x = ., .f = ~ sum(.x, na.rm = TRUE)) %>% enframe() %>% unnest() %>%
                        summarize(sum_n = sum(value, na.rm = TRUE)) %>% pull(sum_n)
                
                # get percentages across all columns
                pct_output_table <- n_output_table %>% select(-c(!!!rows_syms)) %>% 
                        map_dfr(.x = ., .f = ~ .x / sum_n) %>% 
                        bind_cols(n_output_table %>% select(!!!rows_syms), .)
                
                # add stat to variable names
                names(pct_output_table) <- str_replace(string = names(pct_output_table),
                                                       pattern = regex("__n$"), 
                                                       replacement = "__pct")
        }
        
        
        ####################################################################################
        
        
        # get pct_valid_output_table
        
        # get pct_valid_output_table rows
        pct_valid_output_table_rows <- map(.x = rows, 
                     .f = ~ row_index_tbl %>% mutate(row_number = row_number()) %>% 
                     select(row_number, !!sym(.x)) %>% filter(!is.na(!!sym(.x)))) %>% 
                reduce(.x = ., .f = ~ inner_join(.x, .y, by = "row_number"))

        
        
        ################
        
        
        # get pct_valid_output_table cols
        
        # create_output_col_vars function to get final col_vars
        get_na_output_col_vars <- function(.x, col_name_type, current_stat) {
                
                # get current col
                current_col <- .x
                
                # get current_col_var_name_sym
                current_col_var_name_sym <- ensym(current_col)
                
                # get current_col_na_values
                current_col_na_output_table <- n_output_table_initial %>% 
                        mutate(row_number = row_number()) %>%
                        filter(is.na(!!current_col_var_name_sym))
                
                # add current_col to current_col_na_values
                current_col_na_output_table <- current_col_na_output_table %>% mutate(current_col = current_col)
                
                # unite current_col and its values
                current_col_na_output_table <- current_col_na_output_table %>% 
                        unite(col = output_col, current_col, !!current_col_var_name_sym, sep = ".")
                
                # select output_col
                output_col <- current_col_na_output_table %>% select(row_number, output_col)
                return(output_col)
        }
        
        # call get_na_output_col_vars
        na_output_col_vars <- map(.x = cols, 
                                 .f = ~ get_na_output_col_vars(.x, col_name_type = col_name_type, 
                                                               current_stat = "n")) %>%
                enframe() %>% unnest() %>% select(row_number)
        
        # get na_output_col_vars which has final na_output variable name after combining with other groups
        na_output_col_vars <- output_col_vars %>% mutate(row_number = row_number()) %>% 
                left_join(na_output_col_vars, ., by = "row_number") %>%
                select(-row_number) %>%
                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                mutate(col_vars = str_c(col_vars, "__n")) %>% pull(col_vars)

        # get pct_valid_output_table_cols by dropping na cols in na_output_table
        pct_valid_output_col_vars <- n_output_table %>% 
                select(-c(!!!syms(na_output_col_vars))) %>% names()
        
        
        ###################
        
        
        # get n_output_table_for_pct_valid
        n_output_table_for_pct_valid <- n_output_table %>% mutate(row_number = row_number()) %>%
                select(-c(!!!rows_syms)) %>%
                left_join(pct_valid_output_table_rows, ., by = "row_number") %>%
                select(!!!syms(pct_valid_output_col_vars))
        
        
        ####################
        
        
        # with cols_present = FALSE and pct_type = all
        if("pct" %in% stats & cols_present == FALSE & pct_type == "all" & show_valid_pct == TRUE) {
                
                # get pct_output_table
                pct_output_table <- n_output_table_for_pct_valid %>% 
                        mutate(sum_n = sum(n, na.rm = TRUE), pct = n / sum_n) %>%
                        select(-c(n, sum_n))
        }
        
        # with cols_present = TRUE and pct_type = all
        if("pct" %in% stats & cols_present == TRUE & pct_type == "all" & show_valid_pct == TRUE) {
                
                # get sum_n across all columns
                sum_n <- n_output_table_for_pct_valid %>%
                        select(-c(!!!rows_syms)) %>% 
                        map(.x = ., .f = ~ sum(.x, na.rm = TRUE)) %>% enframe() %>% unnest() %>%
                        summarize(sum_n = sum(value, na.rm = TRUE)) %>% pull(sum_n)
                
                # get percentages across all columns
                pct_valid_output_table <- n_output_table_for_pct_valid %>% select(-c(!!!rows_syms)) %>% 
                        map_dfr(.x = ., .f = ~ .x / sum_n) %>% 
                        bind_cols(n_output_table_for_pct_valid %>% select(!!!rows_syms), .)
                
                # add stat to variable names
                names(pct_valid_output_table) <- str_replace(string = names(pct_valid_output_table),
                                                       pattern = regex("__n$"), 
                                                       replacement = "__pct")
        
                # add back the NA cols
                pct_valid_output_table_w_all_cols <- n_output_table %>% select(!!sym(na_output_col_vars)) %>%
                        mutate(row_number = row_number()) %>% 
                        left_join(pct_valid_output_table_rows, ., by = "row_number") %>%
                        select(-c(row_number, !!!rows_syms)) %>%
                        bind_cols(pct_valid_output_table)
                        

                
                # add back the NA rows
                n_output_table %>% mutate(row_number = row_number()) %>% 
                        anti_join(., pct_valid_output_table_rows, by = "row_number") %>%
                        map_at(.x = ., 
                               .at = n_output_table %>% select(-c(!!!rows_syms)) %>% names(), .f = ~ NA) %>%
                        as_tibble() %>% select(-row_number) %>%
                        bind_rows(pct_valid_output_table_w_all_cols, .)
                 
                        
                                
        }
        
        
        ####################################################################################
        
        
        # with cols_present = TRUE and pct_type = row
        if("pct" %in% stats & cols_present == FALSE & pct_type == "row") {
              
                # get pct_output_table
                pct_output_table <- n_output_table %>% mutate(pct = n / n)
        }
        
        # with cols_present = FALSE and pct_type = row
        if("pct" %in% stats & cols_present == TRUE & pct_type == "row") {
        
                # get cols variables from n_output_table that are now united
                non_rows <- tibble(var_names = names(n_output_table)) %>% filter(!(var_names %in% rows)) %>% pull(var_names)
                non_rows_syms <- syms(non_rows)
                
                # get pct_output_table
                pct_output_table <- n_output_table %>% select(!!!non_rows_syms) %>% 
                        rowSums(na.rm = TRUE) %>% tibble(row_sum = .) %>%
                        bind_cols(n_output_table, .) %>% 
                        gather(key = variable, value = value, -c(!!!rows_syms, row_sum)) %>%
                        mutate(value_pct = value / row_sum) %>% select(!!!rows_syms, variable, value_pct) %>%
                        spread(key = variable, value = value_pct)
                
                # add stat to variable names
                names(pct_output_table) <- str_replace(string = names(pct_output_table),
                                                       pattern = regex("__n$"), 
                                                       replacement = "__pct")
        }
        
        
        ######################################################################################3
        
        
        # with cols_present = TRUE and pct_type = col
        if("pct" %in% stats & cols_present == FALSE & pct_type == "col") {
                
                # get pct_output_table
                pct_output_table <- n_output_table %>% mutate(pct = n / sum(n)) %>% select(-n)
        }
        
        # with cols_present = FALSE and pct_type = col
        if("pct" %in% stats & cols_present == TRUE & pct_type == "col") {
                
                # get col var names that are now united
                non_rows <- tibble(var_names = names(n_output_table)) %>% 
                        filter(!(var_names %in% rows)) %>% pull(var_names)
                non_rows_syms <- syms(non_rows)
                
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
                
                # get col_sums_tbl to join with n_output_table for calculating pct
                col_sums_tbl <- map2_dfr(.x = col_sums_table, .y = col_sums_table_vars, .f = get_col_sums_tbl)
                
                # get pct_output_table
                pct_output_table <- n_output_table %>% 
                        gather(key = variable, value = n, -c(!!!rows_syms)) %>%
                        left_join(., col_sums_tbl, by = "variable") %>% mutate(pct = n / sum_n) %>%
                        select(!!!rows_syms, variable, pct) %>%
                        spread(key = variable, value = pct)
                
                # add stat to variable names
                names(pct_output_table) <- str_replace(string = names(pct_output_table),
                                                       pattern = regex("__n$"), 
                                                       replacement = "__pct")
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_n_distinct function
        loop_through_stat_vars_get_n_distinct <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- ensym(current_stat_var)
        
                # get n_distinct_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get n_distinct_output_table
                        n_distinct_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                distinct(!!current_stat_var_sym) %>%
                                count(!!!rows_syms) %>% ungroup() %>%
                                select(-c(!!!rows_syms))
                        
                        # rename stat
                        names(n_distinct_output_table) <- str_c("n_distinct.", current_stat_var)
                }
                
                # get n_distinct_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get n_distinct_output_table
                        n_distinct_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>% 
                                distinct(!!current_stat_var_sym) %>%
                                count(!!!rows_syms, !!!cols_syms) %>% ungroup() %>% 
                                select(-c(!!!cols_syms)) %>% bind_cols(., output_col_vars) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__n_distinct", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = n) %>%
                                select(-c(!!!rows_syms))
                }
                
                return(n_distinct_output_table)
        }
        
        # create n_distinct_output_table 
        if("n_distinct" %in% stats) {
                
                # call loop_through_stat_vars_get_mean
                n_distinct_output_table <- map_dfc(.x = stat_vars, .f = loop_through_stat_vars_get_n_distinct) %>%
                        bind_cols(row_index_tbl, .)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_mean function
        loop_through_stat_vars_get_mean <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- sym(current_stat_var)
                
                # get mean_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get mean_output_table
                        mean_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                summarize(mean = wtd.mean(!!current_stat_var_sym, weights = weights, 
                                                          normwt = normalize_weights, na.rm = TRUE)) %>% 
                                ungroup() %>%  select(-c(!!!rows_syms))

                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("mean.", current_stat_var)
                        current_stat_variable_name_sym <- ensym(current_stat_variable_name)
                        
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
                                summarize(mean = wtd.mean(!!current_stat_var_sym, weights = weights, 
                                                          normwt = normalize_weights, na.rm = TRUE)) %>% 
                                ungroup() %>%
                                select(-c(!!!cols_syms)) %>% bind_cols(., output_col_vars) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__mean", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = mean) %>% 
                                select(-c(!!!rows_syms))
                        
                        return(mean_output_table)
                }
        }
        
        # create mean_output_table 
        if("mean" %in% stats) {
                
                # call loop_through_stat_vars_get_mean
                mean_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_mean) %>%
                        bind_cols(row_index_tbl, .)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_median function
        loop_through_stat_vars_get_median <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- ensym(current_stat_var)
                
                # get median_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get median_output_table
                        median_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                summarize(median = list(enframe(wtd.quantile(!!current_stat_var_sym, 
                                                                                probs = .5, weights = weights, 
                                                                                normwt = normalize_weights, na.rm = TRUE)))) %>% 
                                unnest() %>% ungroup() %>% 
                                mutate(name = str_c("median", ".", current_stat_var)) %>%
                                spread(key = name, value = value) %>%
                                select(-c(!!!rows_syms)) 
                        
                        return(median_output_table)
                }
                
                
                # get median_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get median_output_table
                        median_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>% 
                                summarize(median = list(enframe(wtd.quantile(!!current_stat_var_sym, 
                                                                                probs = .5, weights = weights, 
                                                                                normwt = normalize_weights, na.rm = TRUE)))) %>% 
                                unnest() %>% ungroup() %>% 
                                mutate(name = "median") %>%
                                spread(key = name, value = value) %>% 
                                select(-c(!!!cols_syms)) %>% bind_cols(., output_col_vars) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__median", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = median) %>% 
                                select(-c(!!!rows_syms)) 
                        
                        return(median_output_table)
                }
        }
        
        # create median_output_table 
        if("median" %in% stats) {
                
                # call loop_through_stat_vars_get_median
                median_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_median) %>%
                        bind_cols(row_index_tbl, .)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_mode function
        loop_through_stat_vars_get_mode <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- ensym(current_stat_var)
                
                # get mode_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get mode_output_table, if normalize_weights = TRUE
                        if(normalize_weights == TRUE) {
                                
                                # note that ties for most frequent are broken 
                                # by taking first alphabetical/lowest value of current_stat_var
                                mode_output_table <- data %>% group_by(!!!rows_syms) %>%
                                        count(!!current_stat_var_sym) %>%
                                        group_by(!!!rows_syms) %>%
                                        arrange(desc(n), !!current_stat_var_sym) %>% 
                                        slice(1) %>%
                                        ungroup() %>% select(-n) %>% 
                                        rename(mode = current_stat_var) %>%
                                        select(-c(!!!rows_syms))
                        }
                        
                        # get mode_output_table, if normalize_weights = FALSE
                        # need to multiply observation counts by weights
                        if(normalize_weights == FALSE) {
                                
                                # note that ties for most frequent are broken 
                                # by taking first alphabetical/lowest value of current_stat_var
                                mode_output_table <- data %>% group_by(!!!rows_syms, weights) %>%
                                        count(!!current_stat_var_sym) %>%
                                        ungroup() %>%
                                        mutate(n = n * weights) %>%
                                        # need to sum to account for different weight values
                                        # leading to multiple rows per rows/stat_var combos
                                        group_by(!!!rows_syms, !!current_stat_var_sym) %>%
                                        summarize(n = sum(n, na.rm = TRUE)) %>% ungroup() %>%
                                        group_by(!!!rows_syms) %>%
                                        arrange(desc(n), !!current_stat_var_sym) %>% 
                                        slice(1) %>%
                                        ungroup() %>% select(-n) %>% 
                                        rename(mode = current_stat_var) %>%
                                        select(-c(!!!rows_syms))
                        }
                        
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("mode.", current_stat_var)
                        current_stat_variable_name_sym <- ensym(current_stat_variable_name)
                        
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
                                mode_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>%
                                        count(!!current_stat_var_sym) %>% 
                                        group_by(!!!rows_syms, !!!cols_syms) %>%
                                        arrange(desc(n), !!current_stat_var_sym) %>% slice(1) %>%
                                        ungroup() %>% 
                                        select(-c(!!!cols_syms, n)) %>% bind_cols(., output_col_vars) %>%
                                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                        rename(mode = current_stat_var) %>%
                                        mutate(col_vars = str_c(col_vars, "__mode", ".", current_stat_var)) %>%
                                        spread(key = col_vars, value = mode) %>% 
                                        select(-c(!!!rows_syms))
                        }
                        
                        # get mode_output_table, if normalize_weights = FALSE
                        # need to multiply observation counts by weights
                        if(normalize_weights == FALSE) {
                                
                                # note that ties for most frequent are broken 
                                # by taking first alphabetical/lowest value of current_stat_var
                                mode_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms, weights) %>%
                                        count(!!current_stat_var_sym) %>% 
                                        mutate(n = n * weights) %>%
                                        ungroup() %>% 
                                        # need to sum to account for different weight values
                                        # leading to multiple rows per rows/stat_var combos
                                        group_by(!!!rows_syms, !!!cols_syms, !!current_stat_var_sym) %>%
                                        summarize(n = sum(n, na.rm = TRUE)) %>% ungroup() %>%
                                        group_by(!!!rows_syms, !!!cols_syms) %>%
                                        arrange(desc(n), !!current_stat_var_sym) %>% slice(1) %>%
                                        ungroup() %>% 
                                        select(-c(!!!cols_syms, n)) %>% bind_cols(., output_col_vars) %>%
                                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                        rename(mode = current_stat_var) %>%
                                        mutate(col_vars = str_c(col_vars, "__mode", ".", current_stat_var)) %>%
                                        spread(key = col_vars, value = mode) %>% 
                                        select(-c(!!!rows_syms))
                        }
                        
                        return(mode_output_table)
                }
        }
        
        # create mode_output_table with cols_present = FALSE
        if("mode" %in% stats) {
                
                # call loop_through_stat_vars_get_mode
                mode_output_table <- map_dfc(.x = stat_vars, .f = loop_through_stat_vars_get_mode) %>%
                        bind_cols(row_index_tbl, .)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_sd function
        loop_through_stat_vars_get_sd <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- ensym(current_stat_var)
                
                # get sd_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get sd_output_table
                        sd_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                summarize(sd = sqrt(wtd.var(!!current_stat_var_sym, weights = weights, 
                                                          normwt = normalize_weights, na.rm = TRUE))) %>% 
                                ungroup() %>%  select(-c(!!!rows_syms))
                        
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("sd.", current_stat_var)
                        current_stat_variable_name_sym <- ensym(current_stat_variable_name)
                        
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
                                summarize(sd = sqrt(wtd.var(!!current_stat_var_sym, weights = weights, 
                                                          normwt = normalize_weights, na.rm = TRUE))) %>% 
                                ungroup() %>%
                                select(-c(!!!cols_syms)) %>% bind_cols(., output_col_vars) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__sd", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = sd) %>% 
                                select(-c(!!!rows_syms))
                        
                        return(sd_output_table)
                }
        }
        
        # create sd_output_table with cols_present = FALSE
        if("sd" %in% stats) {
                
                # call loop_through_stat_vars_get_sd
                sd_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_sd) %>%
                        bind_cols(row_index_tbl, .)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_min function
        loop_through_stat_vars_get_min <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- ensym(current_stat_var)
                
                # get min_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get min_output_table
                        min_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                summarize(min = min(!!current_stat_var_sym, na.rm = TRUE)) %>% 
                                ungroup() %>%  select(-c(!!!rows_syms))
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("min.", current_stat_var)
                        current_stat_variable_name_sym <- ensym(current_stat_variable_name)
                        
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
                                summarize(min = min(!!current_stat_var_sym, na.rm = TRUE)) %>% 
                                ungroup() %>%
                                select(-c(!!!cols_syms)) %>% bind_cols(., output_col_vars) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__min", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = min) %>% 
                                select(-c(!!!rows_syms))
                        
                        return(min_output_table)
                }
        }
        
        # create min_output_table with cols_present = FALSE
        if("min" %in% stats) {
                
                # call loop_through_stat_vars_get_min
                min_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_min) %>%
                        bind_cols(row_index_tbl, .)
        }
        
        
        ################################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_max function
        loop_through_stat_vars_get_max <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- ensym(current_stat_var)
                
                # get max_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get max_output_table
                        max_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                summarize(max = max(!!current_stat_var_sym, na.rm = TRUE)) %>% 
                                ungroup() %>%  select(-c(!!!rows_syms))
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("max.", current_stat_var)
                        current_stat_variable_name_sym <- ensym(current_stat_variable_name)
                        
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
                                summarize(max = max(!!current_stat_var_sym, na.rm = TRUE)) %>% 
                                ungroup() %>%
                                select(-c(!!!cols_syms)) %>% bind_cols(., output_col_vars) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__max", ".", current_stat_var)) %>%
                                spread(key = col_vars, value = max) %>% 
                                select(-c(!!!rows_syms))
                        
                        return(max_output_table)
                }
        }
        
        # create max_output_table with cols_present = FALSE
        if("max" %in% stats) {
                
                # call loop_through_stat_vars_get_max
                max_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_max) %>%
                        bind_cols(row_index_tbl, .)
        }
        
        
        ###############################################################################################
        ################################################################################################
        
        
        # create loop_through_stat_vars_get_quantiles function
        loop_through_stat_vars_get_quantiles <- function(.x) {
                
                # get current_stat_var
                current_stat_var <- .x
                
                # get current_stat_var_sym 
                current_stat_var_sym <- ensym(current_stat_var)
                
                # get quantiles_output_table if cols_present = FALSE
                if(cols_present == FALSE) {
                        
                        # get quantiles_order_syms, since by default q100 is place before q25
                        quantiles_order_syms <- syms(str_c("q", round(quantiles, digits = 2) * 100))
                        
                        # get quantiles_output_table
                        quantiles_output_table <- data %>% group_by(!!!rows_syms) %>% 
                                summarize(quantiles = list(enframe(wtd.quantile(!!current_stat_var_sym, 
                                        probs = quantiles, weights = weights, 
                                                          normwt = normalize_weights, na.rm = TRUE)))) %>% 
                                unnest() %>% ungroup() %>% 
                                # mutate(name = str_c("q", str_squish(name)),
                                #        name = str_replace(string = name, pattern = "%", replacement = ""),
                                #        name = str_c(name, ".", current_stat_var)) %>%
                                mutate(name = str_squish(name),
                                       name = str_replace(string = name, pattern = "%", replacement = ""),
                                       name = round(as.numeric(name), digits = 0),
                                       name = as.character(name),
                                       name = str_c("q", str_squish(name))) %>%
                                spread(key = name, value = value) %>%
                                select(-c(!!!rows_syms)) %>% select(!!!quantiles_order_syms)
                                
                        
                        # get current_stat_variable_name
                        current_stat_variable_name <- str_c("quantiles.", current_stat_var)
                        current_stat_variable_name_sym <- ensym(current_stat_variable_name)
                        
                        # rename current_stat_variable_name
                        quantiles_output_table <- quantiles_output_table %>%
                                rename_at(.vars = vars(everything()), 
                                                       .funs = funs(str_c(., ".", current_stat_var))) 
                        
                        return(quantiles_output_table)
                }
                
                
                # get quantiles_output_table if cols_present = TRUE
                if(cols_present == TRUE) {
                        
                        # get collapsed_output_col_vars
                        collapsed_output_col_vars <- output_col_vars %>% 
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>% 
                                distinct(col_vars) %>% arrange(col_vars) %>% pull(col_vars)
                        
                        # get quantiles_order, since by default q100 is place before q25
                        quantiles_order <- syms(str_c("q", round(quantiles, digits = 2) * 100, ".", current_stat_var))
                        
                        # create get_quantiles_ordered_vars function
                        get_quantiles_ordered_vars <- function(.x) {
                                
                                # get current_collapsed_output_col_var
                                current_collapsed_output_col_var <- .x
                                
                                # loop through and join current_collapsed_output_col_var with quantiles_order 
                                current_quantiles_ordered_var <- map(.x = quantiles_order, 
                                    .f = ~ str_c(current_collapsed_output_col_var, "__", .x)) %>%
                                        enframe() %>% unnest() %>% select(value)
                                
                                return(current_quantiles_ordered_var)
                        }
                        
                        # call get_quantiles_ordered_vars
                        quantiles_ordered_vars <- map_dfr(.x = collapsed_output_col_vars, 
                                                          .f = get_quantiles_ordered_vars) %>%
                                pull(value)
                        
                        # get quantiles_ordered_vars_syms
                        quantiles_ordered_vars_syms <- syms(quantiles_ordered_vars)

                        # get quantiles_output_table
                        quantiles_output_table <- data %>% group_by(!!!rows_syms, !!!cols_syms) %>% 
                                summarize(quantiles = list(enframe(wtd.quantile(!!current_stat_var_sym, 
                                                                             probs = quantiles, weights = weights, 
                                                                             normwt = normalize_weights, na.rm = TRUE)))) %>% 
                                unnest() %>% ungroup() %>% 
                                mutate(name = str_squish(name),
                                       name = str_replace(string = name, pattern = "%", replacement = ""),
                                       name = round(as.numeric(name), digits = 0),
                                       name = as.character(name),
                                        name = str_c("q", str_squish(name))) %>%
                                spread(key = name, value = value) %>% 
                                select(-c(!!!cols_syms)) %>% bind_cols(., output_col_vars) %>%
                                unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                mutate(col_vars = str_c(col_vars, "__")) %>%
                                gather(key = quantile, value = value, -c(!!!rows_syms, col_vars)) %>%
                                unite(col = col_vars, col_vars, quantile, sep = "") %>%
                                mutate(col_vars = str_c(col_vars, ".", current_stat_var, sep = "")) %>%
                                spread(key = col_vars, value = value) %>% 
                                select(-c(!!!rows_syms)) %>% select(!!!quantiles_ordered_vars)
                        
                        return(quantiles_output_table)
                }
        }
        
        # create quantiles_output_table 
        if("quantiles" %in% stats) {
                
                # call loop_through_stat_vars_get_quantiles
                quantiles_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_quantiles) %>%
                        bind_cols(row_index_tbl, .)
        }
        
        
        ####################################################################################
        ###################################################################################
        
        
        # handle add_stat_total_row
        
        # create get_stat_total_row function
        get_stat_total_row <- function(.x) {
                
                # get current_stat
                current_stat <- .x
                
                # get current_stat_output_table
                current_stat_output_table <- eval(parse(text = str_c(current_stat, "_output_table")))
                
                # create sum_numeric_cols function
                sum_numeric_cols <- function(.x) {
                        
                        # get current_col
                        current_col <- .x
                        
                        # if current_col is numeric, then sum it; if not, return NA 
                        # this is to handle cases when stat = mode on character class, so summing isn't appropriate
                        if(class(current_col) == "numeric") {
                                return(sum(current_col, na.rm = TRUE))
                        } else {
                                return(NA)
                        }
                }
    
                # get current_stat_total_row
                current_stat_total_row <- current_stat_output_table %>% select(-c(!!!rows_syms)) %>%
                        map(.x = ., .f = sum_numeric_cols) %>% as.tibble() 
                return(current_stat_total_row)
        }

        
        ##############
        
        
        # test
        # n_output_table_wo_cols <- n_output_table
        # n_output_table_w_cols <- n_output_table
        
        # n_output_table <- n_output_table_wo_cols
        # n_output_table <- n_output_table_w_cols
        # 
        # pct_output_table_w_cols <- pct_output_table
        # pct_output_table_wo_cols <- pct_output_table
        # 
        # pct_output_table <- pct_output_table_wo_cols
        # pct_output_table <- pct_output_table_w_cols
        # 
        # current_stat <- "n"
        
        
        ##############
        
        
        # call get_stat_total_row
        if(stat_total_across_groups == FALSE) {
                
                # get stat_total_row
                stat_total_row <- map_dfc(.x = add_stat_total_row, .f = get_stat_total_row) 
                
                # get NA placeholders for row variables
                row_var_placeholder_for_stat_total_row <- map_dfc(.x = rows, 
                                .f = ~ tibble(NA) %>% mutate(!!sym(.x) := NA) %>% select(!!sym(.x))) %>%
                        mutate(!!sym(rows[1]) := "total")
                
                # add row_var_placeholder_for_stat_total_row to stat_total_row
                stat_total_row <- bind_cols(row_var_placeholder_for_stat_total_row, stat_total_row)
        }
        
        
        ###################################################################################
        
        
        # handle add_stat_total_cols
        
        # create get_stat_total_cols function
        get_stat_total_cols<- function(.x) {
                
                # get current_stat
                current_stat <- .x
                
                # get current_stat_output_table
                current_stat_output_table <- eval(parse(text = str_c(current_stat, "_output_table")))
                
                # get current_stat_total_row
                current_stat_total_col <- current_stat_output_table %>% select(-c(!!!rows_syms)) %>%
                        # need to convert any character variables (e.g. stat = mode on character variable)
                        mutate_if(.predicate = is.character, .funs = ~ return(NA)) %>% 
                        summarize(total_col = list(enframe(rowSums(., na.rm = TRUE)))) %>% unnest() %>%
                        mutate(!!sym(str_c(current_stat, "_total")) := value) %>% 
                        select(!!sym(str_c(current_stat, "_total")))
                
                return(current_stat_total_col)
        }
        
        
        ##############
        
        
        # call get_stat_total_cols
        if(stat_total_across_groups == FALSE) {
                
                stat_total_cols <- map_dfc(.x = add_stat_total_cols, .f = get_stat_total_cols) 
        }
        
        
        ###################################################################################
        
        
        # handle when add_stat_total_row and add_stat_total_cols are both not NULL
        # this requires adding to stat_total_row the total of every column in stat_total_cols
        
        if(stat_total_across_groups == FALSE) {
                
                if(!is.null(add_stat_total_row) & !is.null(add_stat_total_cols)) {
                        
                        stat_total_row <- map(.x = stat_total_cols, .f = ~ colSums(tibble(.), na.rm = TRUE)) %>% 
                                as.tibble() %>% bind_cols(stat_total_row, .)
                }
        }
        
        
        ###################################################################################
        ###################################################################################
        ###################################################################################
        
        
        # create final output_table when cols_present = FALSE
        if(cols_present == FALSE) {
                
                # create loop_through_stats_wo_cols function
                loop_through_stats_wo_cols <- function(.x) {
                        
                        # get current_stat
                        current_stat <- .x
                        
                        if(current_stat == "n") {
                                return(n_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "pct") {
                                return(pct_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "n_distinct") {
                                return(n_distinct_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "mean") {
                                return(mean_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "median") {
                                return(median_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "mode") {
                                return(mode_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "sd") {
                                return(sd_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "min") {
                                return(min_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "max") {
                                return(max_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "n_quintiles") {
                                return(n_quintiles_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "quintiles") {
                                return(quintiles_output_table %>% select(-c(!!!rows_syms)))
                        }
                }
                
                # call loop_through_stats_wo_cols
                output_table <- map_dfc(.x = stats, .f = loop_through_stats_wo_cols) 
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
                        # print(str_glue("current_output_col_var is {current_output_col_var}"))
                        
                        # call loop_through_stats function
                        col_var_output <- map_dfc(.x = stats, 
                                .f = ~ loop_through_stats(current_output_col_var = current_output_col_var, .x))
                        # print(names(col_var_output))
                        return(col_var_output)
                }
        
                # create loop_through_stats function
                loop_through_stats <- function(current_output_col_var, .x) {
                        
                        # get current_stat
                        current_stat <- .x
                        
                        # if current_stat = n or pct (which cannot have stat_vars),
                        # then just fetch and return current_output_col_var from n/pct_output_table
                        if(current_stat %in% c("n", "pct")) {
                                
                                # modify current_output_col_var to includ current_stat
                                current_output_col_var <- str_c(current_output_col_var, "__", current_stat)
                                
                                # get current_output_col_var_sym
                                current_output_col_var_sym <- ensym(current_output_col_var)
                                
                                # get current_output_col_var from current_stat output_table
                                if(current_stat == "n") {
                                        current_output <- n_output_table %>% select(!!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "pct") {
                                        current_output <- pct_output_table %>% select(!!current_output_col_var_sym)
                                        return(current_output)                
                                }
                        }
                        
                        # if current_stat != n or pct, then need to loop through stat_vars
                        map_dfc(.x = stat_vars, 
                            .f = ~ loop_through_stat_vars(current_output_col_var = current_output_col_var,
                                                        current_stat = current_stat, .x))
                }
                
                # create loop_through_stat_vars function
                loop_through_stat_vars <- function(current_output_col_var, current_stat, .x) {
                        
                        # get current_stat_var
                        current_stat_var <- .x
                        
                        # modify current_output_col_var to include current_stat_var, 
                        # unless current_stat = "quantiles", which needs special handling due to naming (eg q25)
                        if(current_stat != "quantiles") {
                                current_output_col_var <- str_c(current_output_col_var, "__", current_stat,
                                                        ".", current_stat_var)
                        }
                        
                        # get current_output_col_var_sym - not used to pull quantiles variables though
                        current_output_col_var_sym <- ensym(current_output_col_var)
                        
                        # handle quantiles variables
                        if(current_stat == "quantiles") {
                                current_output_col_vars <- tibble(current_output_col_var = current_output_col_var,
                                       quantiles = str_c("q", round(quantiles, digits = 2) * 100, ".", current_stat_var)) %>%
                                        mutate(current_output_col_var = str_c(current_output_col_var,
                                                quantiles, sep = "__")) %>%
                                        pull(current_output_col_var)
                                
                                # get current_output_col_vars_syms - used to pull quantiles variables
                                current_output_col_vars_syms <- syms(current_output_col_vars)
                        }
                
                        # get current_output_col_var from current_stat output_table 
                        # for any stat_vars, regardless of whether it's character or numeric
                        if(current_stat == "n_distinct") {
                                current_output <- n_distinct_output_table %>% select(!!current_output_col_var_sym)
                                return(current_output)
                        }
                        
                        if(current_stat == "mode") {
                                current_output <- mode_output_table %>% select(!!current_output_col_var_sym)
                                return(current_output)
                        }
                        
                        # get current_output_col_var from current_stat output_table 
                        # only for cases where stat_var is numeric
                        # since current_output_col_var_sym does not exist for these stats when stat_var is character
                        # and this will throw error "<output_col_var> not found"
                        if(current_stat_var %in% stat_vars_numeric) {
                                
                                if(current_stat == "mean") {
                                        current_output <- mean_output_table %>% select(!!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "median") {
                                        current_output <- median_output_table %>% select(!!current_output_col_var_sym)
                                        return(current_output)
                                }
                              
                                if(current_stat == "sd") {
                                        current_output <- sd_output_table %>% select(!!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "min") {
                                        current_output <- min_output_table %>% select(!!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "max") {
                                        current_output <- max_output_table %>% select(!!current_output_col_var_sym)
                                        return(current_output)
                                }
                                if(current_stat == "quantiles") {
                                        current_output <- quantiles_output_table %>% 
                                                select(!!!current_output_col_vars_syms)
                                        return(current_output)
                                }
                        }
                }
                
                # create output_table, minus the row_index_tbl which will be added later below
                output_table <- map_dfc(.x = collapsed_output_col_vars, .f = loop_through_col_vars) 
        }        
        
        # add row_index to output_table
        output_table <- output_table %>% bind_cols(row_index_tbl, .)
        
        
        #################################################################################################
        #################################################################################################
        
        
        # add stat_total_row/cols to output_table
        if(stat_total_across_groups == FALSE) {
                
                output_table <- output_table %>% bind_cols(., stat_total_cols) %>% bind_rows(., stat_total_row)
        }
        
        
        #################################################################################################
        #################################################################################################
        
        
        # handle row_name_type
        
        if(row_name_type == "uncollapsed") {
                return(output_table)
        }
        
        # create get_output_row_names
        get_output_row_var_names <- function(.x, .y) {
                
                # get current_row_var
                current_row_var <- .x
                
                # get current_row_var_name
                current_row_var_name <- .y
                
                # get current_row_var_name_sym
                current_row_var_name_sym <- sym(current_row_var_name)
                
                # get output_row_var_name if row_name_type = "var.value"
                if(row_name_type == "var.value") {
                        
                        # get output_row_var_name
                        current_output_row_var_name <- tibble(current_row_var) %>% 
                                mutate(row_var_name = !!current_row_var_name) %>%
                                unite(col = "output_row_var_name", row_var_name, current_row_var, sep = ".")
                        
                        return(current_output_row_var_name)
                }
                
                # get output_row_var_name if row_name_type = "alue"
                if(row_name_type == "value") {
                        
                        # get output_row_var_name
                        current_output_row_var_name <- tibble(current_row_var) 
                        return(current_output_row_var_name)
                }
        }
        
        output_row_var_names <- output_table %>% select(!!!rows_syms) %>% 
                map2_dfc(.x = ., .y = rows, .f = get_output_row_var_names) %>%
                unite(col = row, sep = "_x_")
        
        output_table <- output_table %>% select(-c(!!!rows_syms)) %>% bind_cols(output_row_var_names, .)
        return(output_table)
}


#############################################################################################


# test tabler
rows <- c("continent", "era")
# cols <- NULL
cols <- c("size", "life_exp")
# weights <- "weights"
weights <- NULL
# stats <- c("n", "pct", "n_distinct", "mean", "median", "mode", "sd", "min", "max", "quantiles")
stats <- c("n", "pct")
# stats <- NULL
stat_vars <- c("pop", "country")
# stat_vars <- c("country", "year")
pct_type <- "all"
n_quantiles <- NULL
# quantiles <- c(0, .25, .5, .75, 1)
quantiles <- c(.25, .5, .75, 1)
add_stat_total_cols <- c("n", "pct")
add_stat_total_row <- c("n", "pct")
stat_total_across_groups <- FALSE
col_name_type <- "var.value.stat.stat_var"
row_name_type <- "value"
normalize_weights <- FALSE
# "n", "pct", "n_distinct", "mean", "median", "mode", "sd", "min", "max", 
# "n_quantiles", "quantiles"
data <- data_original
data <- data %>% mutate(weights = sample(x = seq(from = 1, to = 3, by = .1), size = nrow(data), replace = TRUE))
glimpse(data)

data %>% tabler(rows = rows, cols = cols, weights = weights, stats = stats, stat_vars = stat_vars, 
                pct_type = pct_type, n_quantiles = n_quantiles, quantiles = quantiles, 
                add_stat_total_row = add_stat_total_row, add_stat_total_cols = add_stat_total_cols,
                stat_total_across_groups = FALSE,
                row_name_type = row_name_type, col_name_type = col_name_type,
                normalize_weights = normalize_weights) %>% glimpse()


###########################################################################


# next steps

# valid_pct
# ensure col_name_type is supported - see n_output_table generation
# custom functions
# formatting percents/rounding
# row/col groupings - make row/col arguments accept list
# handling totals across multiple row/col groups


