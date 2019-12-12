library(gapminder)
library(tidyverse)

# https://www.rdocumentation.org/packages/Hmisc/versions/4.1-1/topics/wtd.stats
# https://dplyr.tidyverse.org/articles/programming.html
# http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
# http://pcwww.liv.ac.uk/~william/R/crosstab.r
# https://www.rdocumentation.org/packages/descr/versions/1.1.4/topics/CrossTable
# https://dabblingwithdata.wordpress.com/2017/12/20/my-favourite-r-package-for-frequency-tables/
# https://juba.github.io/questionr/reference/index.html has cross.multi.table()


#################################################################################


# load tabler()
setwd("C:/Users/Stephen/Desktop/R/tabler")
source("tabler.R")


###################################################################################


# load gapminder data
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
stats <- c("n", "pct", "valid_pct", "n_distinct", "mean", "median", "mode", "sd", "min", "max", "quantiles")
# stats <- c("n", "pct", "valid_pct")
# stats <- NULL
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

# test passing a bare variable
data %>% tabler(rows = continent, cols = size, weights = weights, stats = stats, stat_vars = stat_vars, 
                pct_type = pct_type, 
                n_quantiles = n_quantiles, quantiles = quantiles, 
                normalize_weights = normalize_weights)


###########################################################################


# next steps



##############


# prioritized feature list

# add sum to stats!
# add cum_ variants to stats
# allow stat_vars argument to handle c(string), bare, and vars(); like rows/cols
# replace stat = pct and pct_type with stat = c(pct_row, pct_col, pct_all), so they could all be added
# add global na.rm argument, w default FALSE like in base R - decision to toss data needs to be made by human
# add global args na.rm_true/false_stats taking strings to allow for modifying global na.rm arg 
# add a dummy_var_tbl to attributes, so drop_neg_dummies can identify 1/0 dummies (don't include TRUE/FALSE etc)
# complete tabler_tests
# add specify_stat_combos argument 

# allow stacking multiple row/col groups crossed against each other

# add_dummy_vars standalone function (specify vars, converts to format var.value)
# drop_neg_dummy_vars standalone function 
        # (use stored attributes, can drop all, only row/cols, or can also specify vars)
        # will throw warning about not summing to 1 if row/col dummy is dropped, and stats includes pct/total 
# select_rows/cols(); seperate functions for clarity/symmetry; 
        # select by single/multi strings in c(), single bare in c(), single/multi string/bare in vars()
        # full tidyselect helper support in vars()
        # note any rows specified are returned for all row/col_groups to avoid breaking rectangular shape 
        # will throw warning about not summing to 1 or transparently if part of pct/total row/col vars omitted 
# select_stats(); specify stats 
        # pass as string or c(); negation is supported by passing values arg to deparse/sub
        # totals are a stat
        # will throw warning about not summing transparently if part of totals row omitted 
# select_stat_vars(); specify vars 
        # select by single/multi strings in c(), single bare in c(), single/multi string/bare in vars()
        # full tidyselect helper support in vars()
        # negation is supported (for string/bare var use deparse/sub, or negate inside vars())
        # will throw warning about not summing transparently if part of totals row omitted 
# select_row_groups(); (eg groups = 1, groups = c(1, 2), groups = 1:3, groups = c(1, 3:4))
# select_col_groups(); (eg groups = 1, groups = c(1, 2), groups = 1:3, groups = c(1, 3:4))
# select_na(); takes "from" arg with default value c("rows", "cols")
# drop_na(); takes "from" arg with default value c("rows", "cols")
# add_row/col_group_names; row_group names added as variable "row_group" with value 1, 2 etc; 
        # col names appended to end eg var.value__stat.stat_var__col_group_1
        # this updates attribute row/col_group_names = TRUE/FALSE, to allow original var names to be found
# format_comma() specify vars; call repeatedly; default to all numeric vars; wrap mutate_at and scales
# format_dollar() specify vars; call repeatedly; default to all numeric vars; wrap mutate_at and scales
# format_pct(), specify vars; call repeatedly; default to all numeric vars; wrap mutate_at and as_percent
# format_round(), specify vars; call repeatedly; default to all numeric vars; wrap mutate_at and scales
# add_stats_total_row/col standalone functions (can total any stat, even if it's weird like sd)
        # it only makes sense to pass stats args (and the across_groups arg)
        # for multiple row/col groups, totals can recognize row/col groups by attribute tbls
        # the total row/cols can then be added as "totals_row/col_group_1" etc and inserted after row/col
        # or if the argument is passed for across_groups = TRUE, a single "totals_row/col_all_groups" can be added
# add_stats_cumsum_col; takes same target_cols, stats, stat_vars, col_group args as specify_stat_combos
        # cumsum should be done after tabler because it gives chance to drop/reorder rows w/o messing up cumsum
# reorder_rows/cols; order stat_cols based on vars, stats, stat_vars, groups, or specify_vars for full control
        # has across_groups = FALSE default arg
        # reorder_cols can pass multiple criteria; eg by = c("groups", desc("stat_vars"), "stats", "vars")
        # reorder_rows can only take by = "vars" or "groups"
# simplify_col/row_names, drops/adds preceding var., and returns just value__stat.stat_var 
        # (only valid for use on row if row var_names are collapsed and tabler_row is present)
        # this should be run last, since row/col names are no longer recognizable to tabler functions
# collapse_row_names standalone function
# tabler_pivot_longer which uncollapses col names to long data w var, value, stat, and stat_var cols 
# add tabler_to_flextable function that creates spanning headers based on crossed col names
        # nesting order for spanners can just be default order in tabler output, or user can specify alternate
# add tabler_to_gt function that creates spanning headers based on crossed col names
        # nesting order for spanners can just be default order in tabler output, or user can specify alternate

# create package, post on github w readme



