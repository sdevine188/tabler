# need to detach dplyr, since Hmisc summarize function masks it if weights package is loaded after dplyr
# after detaching dplyr, reload tidyverse or dplyr so that instead dplyr's summarize masks Hmisc

# although detaching dplyr may not be possible when dplyr is opened with another package...

# detach_package <- function(pkg, character.only = FALSE) {
#                 
#         if(!character.only) {
#                         pkg <- deparse(substitute(pkg))
#         }
#                 
#         search_item <- paste("package", pkg, sep = ":")
#         while(search_item %in% search()) {
#                         detach(search_item, unload = TRUE, character.only = TRUE)
#         }
# }

detach("package:dplyr", unload=TRUE)


#########################################################################
#########################################################################
#########################################################################


library(Hmisc)
library(dplyr)
library(tidyverse)
library(gapminder)
library(testthat)
library(attempt)
library(rlang)

setwd("C:/Users/Stephen/Desktop/R/tabler")

# https://www.rdocumentation.org/packages/Hmisc/versions/4.1-1/topics/wtd.stats
# https://dplyr.tidyverse.org/articles/programming.html
# http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
# http://pcwww.liv.ac.uk/~william/R/crosstab.r
# https://www.rdocumentation.org/packages/descr/versions/1.1.4/topics/CrossTable
# https://dabblingwithdata.wordpress.com/2017/12/20/my-favourite-r-package-for-frequency-tables/
# https://juba.github.io/questionr/reference/index.html has cross.multi.table()

# create data
# age <- c(10, 20, 30, 10, 20, 30, 30, 30, 30, 20)
# sleep <- c(8, 8, 8, 6, 5, 5, 9, 6, 5, 8)
# country <- c("England", "U.S.", "Germany", "England", "U.S.", "Germany", "U.S.", "U.S.", "U.S.", "England")
# health <- c("high", "medium", "low", "low", "low", "medium", "high", "high", "low", "medium")
# sex <- c("male", "male", "female", "female", "male", "male", "male", "male", "female", "female")
# weights <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
# data <- tibble(age, sleep, country, health, sex, weights)
# data

# also maybe look at gss_cat data in forcats

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
tabler <- function(data, rows, cols = NULL, stats = NULL, stat_vars = NULL, 
                   pct_type = "all", 
                   n_quantiles = NULL, quantiles = c(0, .25, .5, .75, 1), 
                   weights = NULL, normalize_weights = TRUE) {
        
        
        ######################
        
        
        # handle row argument to determine if it's a single bare variable, or 
        # quosures, or character
        
        # will pass row to deparse(substitute()) to see if it's a single bare variable
        # if so, will overwrite row with the deparsed string
        if(deparse(substitute(rows)) %in% names(data)) {
                rows <- deparse(substitute(rows))
        }
        
        # handle row if it's from quo(), quos(), or vars()
        if("quosure" %in% class(rows) | "quosures" %in% class(rows)) {
                
                rows <- map(.x = rows, .f = as_name) %>% unlist()
        }
        
        # if row is not a single bare variable or a quosure, it should be a character
        # the argument tests below will ensure that all variables in row match names in the data
        
        
        #################
        
        
        # handle col argument to determine if it's a single bare variable, or 
        # quosures, or character
        
        # will pass col to deparse(substitute()) to see if it's a single bare variable
        # if so, will overwrite col with the deparsed string
        if(deparse(substitute(cols)) %in% names(data)) {
                cols <- deparse(substitute(cols))
        }
        
        # handle col if it's from quo(), quos(), or vars()
        if("quosure" %in% class(cols) | "quosures" %in% class(cols)) {
                
                cols <- map(.x = cols, .f = as_name) %>% unlist()
        }
        
        # if col is not a single bare variable or a quosure, it should be a character
        # the argument tests below will ensure that all variables in col match names in the data
        
        
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
        
        # set stat to default c("n", "pct", "valid_pct") if no stat arguments were passed and cols_present = FALSE
        if(is.null(stats) & cols_present == FALSE) {
                stats <- c("n", "pct", "valid_pct")
        } 
        
        
        ############################################################################################
        
        
        # check arguments
        if(missing(data)) {
                stop("The data argument is required, but no input was given.")
        }
        
        if(missing(rows)) {
                stop("The rows argument is required, but no input was given.")
        }
        
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
        
        if(sum(stats %in% c("n", "pct", "valid_pct", "n_distinct", "mean", "median", "mode", "sd", "min", "max", 
                           "quantiles")) != length(stats)) {
                stop("The stats argument can only include one or more of the following: 
                     n', 'pct', 'valid_pct', 'n_distinct', 'mean', 'median', 'mode', 'sd', 'min', 'max', 
                     'quantiles'.  Default is c('n', 'pct').")
        }
        
        if(!pct_type %in% c("all", "row", "col")) {
                stop("The pct_type argument must be either 'all' (default), 'row', or 'col'.")
        }
        
        if("quantiles" %in% stats & is.null(quantiles)) {
                stop("The string 'quantiles' was passed to the stat argument, ",
                     "but the quantiles argument was set to NULL. ",
                     "If 'quantiles' is passed to the stat argument, ",
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
        
        # if(sum(add_stat_total_col %in% c("n", "pct", "n_distinct", "mean", "median", "mode", "sd", "min", "max", 
        #                                  "quantiles")) != length(add_stat_total_col)) {
        #         stop("The add_stat_total_col argument can only include one or more of the following: 
        #              n', 'pct', 'valid_pct', 'n_distinct', 'mean', 'median', 'mode', 'sd', 'min', 'max', 
        #              'quantiles'.  Default is NULL.")
        # }
        
        if(!normalize_weights %in% c(TRUE, FALSE)) {
                stop("The normalize_weights argument must be either TRUE or FALSE (default).")
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
        create_output_vars <- function(.x, n_output_table_initial) {
                
                # get current__var_name
                current_var_name <- .x
                
                # get current__var_name_sym
                current_var_name_sym <- ensym(current_var_name)
                
                # add current_ to n_output_table
                current_n_output_table <- n_output_table_initial %>% mutate(current_var = current_var_name)
                
                # unite current_ and its values
                current_n_output_table <- current_n_output_table %>% 
                        unite(col = output_var, current_var, !!current_var_name_sym, sep = ".")
                
                # select output_
                output_var <- current_n_output_table %>% select(output_var)
                return(output_var)
        }
        
        # with cols_present = FALSE
        if(cols_present == FALSE) {
                
                # get n_output_table
                n_output_table <- data %>% group_by(!!!rows_syms) %>% count(wt = weights) %>% ungroup() 
                
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
                output_col_vars <- map_dfc(.x = cols, 
                                           .f = ~create_output_vars(.x, 
                                                        n_output_table_initial = n_output_table_initial)) %>% 
                        rename_at(.vars = vars(contains("output_var")), 
                                .funs = ~ str_replace(string = ., pattern = "output_var", replacement = "output_col"))

                # get final_output_col_vars, which collapses the output_col to how they'll look in final output
                final_output_col_vars <- output_col_vars %>% 
                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                        distinct(col_vars)
                
                # get row_index_key_tbl
                row_index_key_tbl <- n_output_table_initial %>% select(-n) %>% bind_cols(., output_col_vars) %>% 
                        unite(col = row_index_key, !!!rows_syms, !!!cols_syms, sep = "_", remove = FALSE) %>% 
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
        
        
        # get metadata to save as attributes
        
        
        # use row_index_key_tbl to create row_var_tbl and col_var_tbl
        if(cols_present == TRUE) {
                # create col_var_tbl
                col_var_tbl <- map_dfc(.x = cols, .f = ~create_output_vars(.x,
                                                n_output_table_initial = n_output_table_initial)) %>% distinct() %>%
                        rename_at(.vars = vars(contains("output_var")), 
                                .funs = ~ str_replace(string = ., pattern = "output_var", 
                                                      replacement = "output_col")) %>%
                        bind_cols(row_index_key_tbl %>% select(!!!cols_syms) %>% distinct(), .) %>%
                        unite(col = col_vars, starts_with("output_col"), sep = "_x_", remove = FALSE) %>% 
                        select(col_vars, everything()) %>% select(2:ncol(.), 1)
        } else {
                col_var_tbl <- NA
        }
        
        
        ###################
        

        # create row_var_tbl

        # get output_row_vars 
        output_row_vars <- map_dfc(.x = rows, 
                                   .f = ~create_output_vars(.x, n_output_table_initial = n_output_table_initial)) %>%
                rename_at(.vars = vars(contains("output_var")), 
                          .funs = ~ str_replace(string = ., pattern = "output_var", replacement = "output_row"))
        
        # get final_output_row_vars, which collapses the output_row to how they'll look in final output
        final_output_row_vars <- output_row_vars %>% 
                unite(col = row_vars, starts_with("output_row"), sep = "_x_") %>%
                distinct(row_vars)
        
        # get row_var_tbl
        row_var_tbl <- n_output_table %>% select(!!!rows_syms) %>% bind_cols(., output_row_vars %>% distinct()) %>% 
                bind_cols(., final_output_row_vars)
        
        
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
        
        
        # with cols_present = TRUE and pct_type = row
        if("pct" %in% stats & cols_present == FALSE & pct_type == "row") {
                
                # get pct_output_table
                pct_output_table <- n_output_table %>% mutate(pct = n / n) %>% select(-n)
        }
        
        # with cols_present = FALSE and pct_type = row
        if("pct" %in% stats & cols_present == TRUE & pct_type == "row") {
                
                # get col variables from n_output_table that are now united
                non_row <- tibble(var_names = names(n_output_table)) %>% filter(!(var_names %in% rows)) %>% pull(var_names)
                non_rows_syms <- syms(non_row)
                
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
        
        
        ####################################################################################
        ######################################################################################
        
        
        # get valid_pct_output_table
        
        # get valid_pct_output_table_row
        if("valid_pct" %in% stats) {
                
                # get valid_pct_output_table row
                valid_pct_output_table_row <- map(.x = rows, 
                                                  .f = ~ row_index_tbl %>% mutate(row_number = row_number()) %>% 
                                                          select(row_number, !!sym(.x)) %>% filter(!is.na(!!sym(.x)))) %>% 
                        reduce(.x = ., .f = ~ inner_join(.x, .y, by = "row_number"))
        }
        
        
        ################
        
        
        # if cols_present = FALSE and pct_type = "all"
        if("valid_pct" %in% stats & cols_present == FALSE) {
                
                # get n_output_table_for_valid_pct
                n_output_table_for_valid_pct <- n_output_table %>% mutate(row_number = row_number()) %>%
                        select(-c(!!!rows_syms)) %>%
                        left_join(valid_pct_output_table_row, ., by = "row_number")
                
                # get valid_pct_output_table, depending on pct_type
                if(pct_type == "all") {
                        
                        # get valid_pct_output_table
                        valid_pct_output_table <- n_output_table_for_valid_pct %>% 
                                mutate(sum_n = sum(n, na.rm = TRUE), valid_pct = n / sum_n) %>%
                                select(-c(n, sum_n))
                        
                } else if(pct_type == "row") {
                        
                        # get valid_pct_output_table
                        valid_pct_output_table <- n_output_table_for_valid_pct %>% mutate(valid_pct = n / n)
                        
                } else if(pct_type == "col") {
                        
                        # get valid_pct_output_table
                        valid_pct_output_table <- n_output_table_for_valid_pct %>% 
                                mutate(valid_pct = n / sum(n)) %>% select(-n)
                        
                }
                
                # add back the NA row
                valid_pct_output_table <- n_output_table %>% mutate(row_number = row_number()) %>% 
                        anti_join(., valid_pct_output_table_row, by = "row_number") %>%
                        map_at(.x = ., 
                               .at = n_output_table %>% select(-c(!!!rows_syms)) %>% names(), .f = ~ NA) %>%
                        as_tibble() %>% select(-row_number) %>% rename(valid_pct = n) %>%
                        bind_rows(valid_pct_output_table, .) %>% select(-row_number)
                
                # reorder row of valid_pct_output_table based on n_output_table
                valid_pct_output_table <- valid_pct_output_table %>%  
                        unite(col = united_row_index, !!!rows_syms, remove = FALSE) %>%
                        left_join(n_output_table %>% unite(col = united_row_index, !!!rows_syms) %>% 
                                          select(united_row_index), ., by = "united_row_index") %>%
                        select(-united_row_index)
        }
        
        
        #################
        
        
        # create get_na_output_col_vars function 
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
        
        
        # get valid_pct_output_col_vars, for use calculating valid_pct regardless of whether pct_type = all/row/col
        if("valid_pct" %in% stats & cols_present == TRUE) {
                
                # call get_na_output_col_vars
                na_output_col_vars <- map(.x = cols,
                                          .f = ~ get_na_output_col_vars(.x, current_stat = "n")) %>%
                        enframe() %>% unnest() %>% select(row_number)
                
                # get na_output_col_vars which has final na_output variable name after combining with other groups
                na_output_col_vars <- output_col_vars %>% mutate(row_number = row_number()) %>%
                        left_join(na_output_col_vars, ., by = "row_number") %>%
                        select(-row_number) %>%
                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                        mutate(col_vars = str_c(col_vars, "__n")) %>% pull(col_vars)
                
                # get valid_pct_output_table_col by dropping na col in na_output_table
                valid_pct_output_col_vars <- n_output_table %>%
                        select(-c(!!!syms(na_output_col_vars))) %>% names()
        }
        
        
        ##########################
        
        
        # if cols_present = TRUE and pct_type = "all"
        if("valid_pct" %in% stats & cols_present == TRUE) {
                
                # get n_output_table_for_valid_pct
                n_output_table_for_valid_pct <- n_output_table %>% mutate(row_number = row_number()) %>%
                        select(-c(!!!rows_syms)) %>%
                        left_join(valid_pct_output_table_row, ., by = "row_number") %>%
                        select(!!!syms(valid_pct_output_col_vars))
                
                if(pct_type == "all") {
                        
                        # get sum_n across all columns
                        sum_n <- n_output_table_for_valid_pct %>%
                                select(-c(!!!rows_syms)) %>%
                                map(.x = ., .f = ~ sum(.x, na.rm = TRUE)) %>% enframe() %>% unnest() %>%
                                summarize(sum_n = sum(value, na.rm = TRUE)) %>% pull(sum_n)
                        
                        # get percentages across all columns
                        valid_pct_output_table <- n_output_table_for_valid_pct %>% select(-c(!!!rows_syms)) %>%
                                map_dfr(.x = ., .f = ~ .x / sum_n) %>%
                                bind_cols(n_output_table_for_valid_pct %>% select(!!!rows_syms), .)
                        
                } else if(pct_type == "row") {
                        
                        # get col variables from n_output_table that are now united
                        non_row <- tibble(var_names = names(n_output_table_for_valid_pct)) %>% 
                                filter(!(var_names %in% rows)) %>% pull(var_names)
                        non_rows_syms <- syms(non_row)
                        
                        # get valid_pct_output_table
                        valid_pct_output_table <- n_output_table_for_valid_pct %>% select(!!!non_rows_syms) %>%
                                rowSums(na.rm = TRUE) %>% tibble(row_sum = .) %>%
                                bind_cols(n_output_table_for_valid_pct, .) %>%
                                gather(key = variable, value = value, -c(!!!rows_syms, row_sum)) %>%
                                mutate(value_pct = value / row_sum) %>% select(!!!rows_syms, variable, value_pct) %>%
                                spread(key = variable, value = value_pct)
                        
                } else if(pct_type == "col") {
                        
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
                        col_sums_tbl <- map2_dfr(.x = col_sums_table, .y = col_sums_table_vars, .f = get_col_sums_tbl)
                        
                        # get pct_output_table
                        valid_pct_output_table <- n_output_table_for_valid_pct %>% 
                                gather(key = variable, value = n, -c(!!!rows_syms)) %>%
                                left_join(., col_sums_tbl, by = "variable") %>% mutate(pct = n / sum_n) %>%
                                select(!!!rows_syms, variable, pct) %>%
                                spread(key = variable, value = pct)
                        
                }
                
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
                names(valid_pct_output_table) <- valid_pct_output_table %>%
                        select(-c(!!!rows_syms)) %>% names() %>%
                        str_replace(string = ., pattern = regex("__n$"), replacement = "__valid_pct") %>%
                        c(rows, .)
                
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
                n_distinct_output_table <- map_dfc(.x = stat_vars, .f = loop_through_stat_vars_get_n_distinct) 
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
                mean_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_mean) 
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
                median_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_median) 
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
                                        # leading to multiple row per row/stat_var combos
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
                                        unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                        left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)), 
                                                  by = "row_index_key") %>%
                                        arrange(row_index_tbl_row_number) %>% 
                                        select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
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
                                        # leading to multiple row per row/stat_var combos
                                        group_by(!!!rows_syms, !!!cols_syms, !!current_stat_var_sym) %>%
                                        summarize(n = sum(n, na.rm = TRUE)) %>% ungroup() %>%
                                        group_by(!!!rows_syms, !!!cols_syms) %>%
                                        arrange(desc(n), !!current_stat_var_sym) %>% slice(1) %>%
                                        ungroup() %>% select(-n) %>%
                                        unite(col = row_index_key, !!!rows_syms, !!!cols_syms, remove = FALSE) %>%
                                        left_join(., row_index_key_tbl %>% select(-c(!!!rows_syms), -c(!!!cols_syms)),
                                                  by = "row_index_key") %>%
                                        arrange(row_index_tbl_row_number) %>%
                                        select(-c(row_index_key, row_index_tbl_row_number, !!!cols_syms)) %>%
                                        unite(col = col_vars, starts_with("output_col"), sep = "_x_") %>%
                                        rename(mode = current_stat_var) %>%
                                        mutate(col_vars = str_c(col_vars, "__mode", ".", current_stat_var)) %>%
                                        spread(key = col_vars, value = mode)
                        }
                        
                        return(mode_output_table)
                }
        }
        
        # create mode_output_table with cols_present = FALSE
        if("mode" %in% stats) {
                
                # call loop_through_stat_vars_get_mode
                mode_output_table <- map_dfc(.x = stat_vars, .f = loop_through_stat_vars_get_mode) 
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
                                summarize(sd = suppressWarnings(sqrt(wtd.var(!!current_stat_var_sym, 
                                                                             weights = weights, 
                                                                             normwt = normalize_weights, na.rm = TRUE)))) %>% 
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
                                summarize(sd = suppressWarnings(sqrt(wtd.var(!!current_stat_var_sym, 
                                                                             weights = weights, 
                                                                             normwt = normalize_weights, na.rm = TRUE)))) %>% 
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
                sd_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_sd) 
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
                min_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_min)
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
                max_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_max)
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
                                spread(key = col_vars, value = value) %>% 
                                select(!!!quantiles_ordered_vars)
                        
                        return(quantiles_output_table)
                }
        }
        
        # create quantiles_output_table 
        if("quantiles" %in% stats) {
                
                # call loop_through_stat_vars_get_quantiles
                quantiles_output_table <- map_dfc(.x = stat_vars_numeric, .f = loop_through_stat_vars_get_quantiles) 
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
                                return(n_output_table %>% select(-c(!!!rows_syms)))
                        }
                        if(current_stat == "pct") {
                                return(pct_output_table %>% select(-c(!!!rows_syms)))
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
                        if(current_stat == "quantiles") {
                                return(quantiles_output_table)
                        }
                }
                
                # call loop_through_stat_wo_col
                output_table <- map_dfc(.x = stats, .f = loop_through_stat_wo_col) 
                
                # add row_index to output_table
                # note i need to count() row_index_tbl to arrange any NA values to be sorted first
                # but arrange() always places NA values last, even arrange(desc())
                output_table <- output_table %>% 
                        bind_cols(row_index_tbl %>% count(!!!rows_syms) %>% select(-n), .) %>%
                        arrange(!!!rows_syms)
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
                        col_var_output <- map_dfc(.x = stats, 
                                .f = ~ loop_through_stat(current_output_col_var = current_output_col_var, .x))
                        return(col_var_output)
                }
                
                # create loop_through_stat function
                loop_through_stat <- function(current_output_col_var, .x) {
                        
                        # get current_stat
                        current_stat <- .x
                        
                        # if current_stat = n or pct (which cannot have stat_vars),
                        # then just fetch and return current_output_col_var from n/pct_output_table
                        if(current_stat %in% c("n", "pct", "valid_pct")) {
                                
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
                                if(current_stat == "valid_pct") {
                                        current_output <- valid_pct_output_table %>% 
                                                select(!!current_output_col_var_sym)
                                        return(current_output)                
                                }
                        }
                        
                        # if current_stat != n, pct, or valid_pct, then need to loop through stat_vars
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
                        # since current_output_col_var_sym does not exist for these stat when stat_var is character
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
                
                # add row_index to output_table
                output_table <- output_table %>% bind_cols(row_index_tbl, .)
        }        
        
        
        #################################################################################################
        #################################################################################################
        
        
        # add attributes to facilitate tabler helper functions
        
        # add rows and cols as attributes
        attr(x = output_table, which = "tabler_rows") <- rows
        attr(x = output_table, which = "tabler_cols") <- cols
        
        # add stats as attributes
        attr(x = output_table, which = "tabler_stats") <- stats
        
        # add stat_vars as attributes
        attr(x = output_table, which = "tabler_stat_vars") <- stat_vars
        
        # add row_var_tbl and col_var_tbl as attributes 
        # this enables tabler_pivot_longer() and tabler_uncollapse/collapse_row_names()
        attr(x = output_table, which = "tabler_row_var_tbl") <- row_var_tbl
        attr(x = output_table, which = "tabler_col_var_tbl") <- col_var_tbl
        
        
        #################################################################################################
        #################################################################################################
        
        
        # return output table
        return(output_table)
}



#############################################################################################


# test tabler
# rows <- c("continent", "era")
rows <- vars(continent, era)
# rows <- "continent"
# cols <- NULL
# cols <- c("size", "life_exp")
cols <- vars(size, life_exp)
# weights <- "weights"
weights <- NULL
# stats <- c("n", "pct", "valid_pct", "n_distinct", "mean", "median", "mode", "sd", "min", "max", "quantiles")
# stats <- c("n", "pct", "valid_pct")
stats <- NULL
# stat_vars <- NULL
stat_vars <- c("pop", "country")
# stat_vars <- c("country", "year")
pct_type <- "row"
n_quantiles <- NULL
# quantiles <- c(0, .25, .5, .75, 1)
quantiles <- c(.25, .5, .75, 1)
normalize_weights <- FALSE

data <- data_original
# data <- data %>% mutate(weights = sample(x = seq(from = 1, to = 3, by = .1), size = nrow(data), replace = TRUE))
glimpse(data)

tabler_output <- data %>% tabler(rows = rows, cols = cols, weights = weights, stats = stats, stat_vars = stat_vars, 
                                 pct_type = pct_type, 
                                 n_quantiles = n_quantiles, quantiles = quantiles, 
                                 normalize_weights = normalize_weights) 
tabler_output %>% glimpse()
tabler_output %>% attributes()
# write_csv(tabler_output, path = "tabler_output.csv")

data %>% tabler(row = "continent", weights = weights, stat = stat, stat_vars = stat_vars, 
                pct_type = pct_type, 
                n_quantiles = n_quantiles, quantiles = quantiles, 
                normalize_weights = normalize_weights)


###########################################################################


# next steps



##############


# prioritized feature list

# consider add row_number to tbls when doing spread(), since if there are otherwise duplicate records it has issues??
# complete tabler_tests
# add specify_stat_combos argument 

# allow stacking multiple row/col groups crossed against each other

# tabler_create_dummies standalone function (convert w/ format var.value)
# tabler_drop_neg_dummies standalone function (use stored attributes, can drop all, or can also specify vars)
# tabler_comma_format() standalone function w/ round option
# tabler_pct_format() standalone function that specifically targets pct vars
# tabler_round_format() standalone function
# tabler_custom_format standalone function that can apply comma/pct/round format to specific cols
# tabler_stat_total_row/col standalone functions (can total any stat)
# for multiple row/col groups, totals can recognize row/col groups by attribute tbls
# the total row/cols can then be added as "totals_row/col_group_1" etc and inserted after row/col
# or if the argument is passed for across_groups = TRUE, a single "totals_row/col_all_groups" can be added
# tabler_col_reorder standalone function to order stat_cols based on col_vars, stats, or stat_vars
# tabler_simplify_col/row_names, drops preceding var., and returns just value__stat.stat_var 
# (only valid for use on row if row var_names are collapsed and tabler_row is present)
# tabler_collapse_row_names standalone function
# tabler_uncollapse_row_names standalone function
# tabler_pivot_longer which uncollapses col names to long data w var, value, stat, and stat_var cols (use attributes)

# add tabler_to_flextable function that creates spanning headers based on grouped column names
# nesting order can just be default order in tabler output, or user can specify alternate

# allow for custom functions to be added to stat argument?? maybe not wise


