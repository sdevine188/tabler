
# test tabler function
# note: tabler function should already be loaded before running tabler_tests

# the goal of tabler function is to make it easier to build tables: 
# 1) summarizing multiple statistics,
# 2) for multiple variables,
# 3) within multiple cross-tab categories spread across the rows and/or columns,
# 4) and binding together multiple groups of such tables,
# 5) along with convenient helper functions to refine table presentation

# tabler(data, rows, cols, stats, stat_vars, 
#        pct_type = pct_type, show_valid_pct = show_valid_pct, 
#        normalize_weights = normalize_weights)

# get data from gss_cat in forcats package
data <- gss_cat %>% mutate(party = case_when(partyid %in% c("Strong republican", "Not str republican",
                                                            "Ind, near rep") ~ "Republican",
                                             partyid %in% c("Strong democrat", "Not str democrat", 
                                                            "Ind, near dem") ~ "Democrat", TRUE ~ "Independent")) %>%
        filter(marital %in% c("Married", "Divorced"), race %in% c("White", "Black"),
               relig %in% c("Catholic", "Protestant"), partyid %in% c("Strong republican", "Strong democrat"))

# inspect data
data %>% count(marital)
data %>% count(race)
data %>% count(rincome)
data %>% count(year)
data %>% count(relig)
data %>% count(partyid)
data %>% count(party)


#######################################################################################################


# confirm that tabler can handle spaces in variable names and spaces in values 
# note converting tbl to data.frame() automatically replaces colons and spaces in variable names with dots

# var/values w spaces as rows
spaces_as_rows <- data %>%
        rename("relig var" = relig) %>%
        tabler(rows = c("partyid", "relig var"), cols = vars(race, marital), stats = "n")
spaces_as_rows
spaces_as_rows %>% data.frame()

# var/values w spaces as cols
spaces_as_cols <- data %>% 
        rename("relig var" = relig) %>%
        tabler(rows = vars(marital, race), cols = c("relig var", "partyid"), stats = "n")
spaces_as_cols
spaces_as_cols %>% data.frame()


########################################################################################
#########################################################################################


# create manual_n_table
data %>% mutate(relig.Catholic_x_marital.Divorced)


manual_n_table <- data %>% 
        group_by(race, party, relig, marital) %>%
        count() %>% ungroup() %>%
        unite(col = "col_var", relig, marital, sep = "_x_") %>%
        pivot_wider(names_from = col_var, values_from = n) %>%
        # convert all numerics to double for easy comparison
        map_dfc(.x = ., .f = function(.x) { 
                if(is.numeric(.x)) {.x * 1.0} 
                else {.x}
        })
manual_n_table %>% data.frame()

# create tabler_n_table
tabler_n_table <- data %>% 
        tabler(rows = vars(race, party), cols = vars(relig, marital), stats = "n") %>%
        map_dfc(.x = ., .f = function(.x) { 
                if(is.numeric(.x)) {.x * 1.0} 
                else {.x}
        })

# rename output vars to be same as manual table for easy comparison
tabler_n_table %>% data.frame()     
names(tabler_n_table) <- names(manual_n_table) 
tabler_n_table %>% data.frame()     

# compare manual table with tabler
identical(manual_n_table, tabler_n_table)


########################################################################################
#########################################################################################




########################################################################################
#########################################################################################
########################################################################################


# read in large dataset to test tabler
setwd("C:/Users/Stephen/Desktop/R/tabler")
library(tidyverse)
library(lubridate)

# cfpb <- read_csv("data/Consumer_Complaints.csv") %>%
#         mutate(quant_var = sample(x = 1:10, size = nrow(.), replace = TRUE)) %>%
#         rename(submitted_via = "Submitted via", product = Product, timely_response = "Timely response?") %>%
#        mutate(year = year(mdy(`Date received`)))
cfpb %>% glimpse()
cfpb %>% dim()

cfpb %>% count(product)
cfpb %>% tabler(rows = product)

cfpb %>% tabler(rows = product, cols = submitted_via) %>% glimpse()
# need to debug: valid_pct, mode
cfpb %>% tabler(rows = product, cols = submitted_via, 
                stats = c("n", "pct", "valid_pct", "mean", "median", "mode",
                          "sd", "min", "max", "quantiles", "n_distinct"), 
                                   stat_vars = "quant_var") %>% glimpse()

cfpb %>% tabler(rows = product, cols = vars(submitted_via, timely_response), 
                stats = c("n", "pct", "valid_pct", "mean", "median", "mode",
                          "sd", "min", "max", "quantiles", "n_distinct"),
                stat_vars = "quant_var") %>% glimpse()

cfpb %>% tabler(rows = vars(year, product), cols = vars(submitted_via, timely_response), 
                stats = c("n", "pct", "valid_pct", "mean", "median", "mode", 
                          "sd", "min", "max", "quantiles", "n_distinct"),
                stat_vars = "quant_var") %>% glimpse()


###########################


# manually test tabler using cfpb
data <- cfpb
rows <- vars(product)
# rows <- vars(year, product)
# cols <- vars(submitted_via, timely_response)
cols <- vars(submitted_via)
# stats <- c("n", "pct", "valid_pct", "mean", "mode", "median", "sd", "min", "max", "quantiles", "n_distinct")
stats <- "mode"
stat_vars <- "quant_var"
pct_type <- "all"
quantiles <- c(0, .25, .5, .75, 1)
n_quantiles <- NULL
weights <- NULL
normalize_weights <- TRUE
