library(tidyverse)
library(gapminder)


# https://www.rdocumentation.org/packages/Hmisc/versions/4.1-1/topics/wtd.stats
# https://dplyr.tidyverse.org/articles/programming.html
# http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
# http://pcwww.liv.ac.uk/~william/R/crosstab.r
# https://www.rdocumentation.org/packages/descr/versions/1.1.4/topics/CrossTable
# https://dabblingwithdata.wordpress.com/2017/12/20/my-favourite-r-package-for-frequency-tables/
# https://juba.github.io/questionr/reference/index.html has cross.multi.table()


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
                                      continent = as.character(continent),
                                      sport = case_when(gdpPercap < 1000 ~ "track",
                                                        gdpPercap >= 1000 & gdpPercap < 10000 ~ "soccer",
                                                        gdpPercap >= 10000 ~ "basketball", 
                                                        TRUE ~ NA_character_)) %>% 
        filter(country != "Cote d'Ivoire") %>%
        mutate(row_number = row_number(), 
               continent = case_when(row_number == 1 ~ NA_character_, TRUE ~ continent),
               size = case_when(row_number == 2 ~ NA_character_, TRUE ~ size))

data_original %>% glimpse()


#################################################################################


# load tabler()
setwd("C:/Users/Stephen/Desktop/R/tabler")
source("tabler.R")


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
stats <- c("n", "pct_col", "pct_row", "pct_all", "valid_pct_col", "valid_pct_row", "valid_pct_all", 
           "n_distinct", "sum", "mean", "median", "mode", "sd", "min", "max", "quantile")
# stats <- c("n", "pct", "valid_pct")
# stats <- NULL
# stat_vars <- NULL
stat_vars <- c("pop", "country", "sport", "gdpPercap")
# stat_vars <- c("country", "year")
pct_type <- "row"
n_quantiles <- NULL
# quantiles <- c(0, .25, .5, .75, 1)
quantile <- c(.25, .5, .75, 1)
normalize_weights <- FALSE

data <- data_original
# data <- data %>% mutate(weights = sample(x = seq(from = 1, to = 3, by = .1), size = nrow(data), replace = TRUE))
glimpse(data)

tabler_output <- data %>% tabler(rows = rows, cols = cols, weights = weights, stats = stats, 
                                 stat_vars = stat_vars, quantile = quantile, 
                                 normalize_weights = normalize_weights) 
tabler_output %>% dim()
tabler_output %>% glimpse()
tabler_output %>% attributes()
# write_csv(tabler_output, path = "tabler_output.csv")

# test passing a bare variable
data %>% tabler(rows = continent, cols = size, weights = weights, stats = stats, stat_vars = vars(pop, country), 
                pct_type = pct_type, quantile = quantile, 
                normalize_weights = normalize_weights) %>% glimpse()


###########################################################################


# next steps


##############


# prioritized feature list

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
        # will throw warning about not summing transparently if pct_row/total/cumsum involved
# select_row_groups(); (eg groups = 1, groups = c(1, 2), groups = 1:3, groups = c(1, 3:4))
# select_col_groups(); (eg groups = 1, groups = c(1, 2), groups = 1:3, groups = c(1, 3:4))
# select_na(); takes "from" arg with default value c("rows", "cols");
# drop_na(); takes "from" arg with default value c("rows", "cols")
        # will throw warning about not summing transparently if pct_row/total/cumsum involved
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
        # will flag warning if reorder_rows called when stat_cumsum_col is present
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



#######################################################################################################
#######################################################################################################
#######################################################################################################


# notes on weights

# see docs for wtd.mean
# In most cases the weights vector is a vector the same length of x, 
# containing frequency counts that in effect expand x by these counts. 
# weights can also be sampling weights, in which setting normwt to TRUE will often be appropriate. 
# This results in making weights sum to the length of the non-missing elements in x. normwt=TRUE 
# thus reflects the fact that the true sample size is the length of the x vector and 
# not the sum of the original values of weights (which would be appropriate had normwt=FALSE). W
# hen weights is all ones, the estimates are all identical to unweighted estimates 
# (unless one of the non-default quantile estimation options is specified to wtd.quantile)

# and the docs define the normwt arg:
# specify normwt=TRUE to make weights sum to length(x) after deletion of NAs. 
# If weights are frequency weights, then normwt should be FALSE, 
# and if weights are normalization (aka reliability) weights, then normwt should be TRUE.

# and note that normwt arg is ignored for wtd.mean, because the concept of normalizing doesn't affect calculation
# wtd.mean(x, weights=NULL, normwt="ignored", na.rm=TRUE)

# show that wtd.mean is unaffected by normwt
df <- starwars %>% select(species, mass) %>% 
        mutate(weights = c(rep(2, times = 5), rep(1, times = (nrow(.) - 5))))
df

# null weights
wtd.mean(x = df %>% pull(mass), 
         weights = NULL, 
         normwt = TRUE, na.rm = TRUE)
wtd.mean(x = df %>% pull(mass), 
         weights = NULL, 
         normwt = FALSE, na.rm = TRUE)
df %>% summarize(mean = mean(mass, na.rm = TRUE))

# weights = 1
wtd.mean(x = df %>% pull(mass), 
         weights = starwars %>% mutate(weights = 1) %>% pull(weights), 
         normwt = TRUE, na.rm = TRUE)
wtd.mean(x = df %>% pull(mass), 
         weights = starwars %>% mutate(weights = 1) %>% pull(weights), 
         normwt = FALSE, na.rm = TRUE)
df %>% summarize(mean = mean(mass, na.rm = TRUE))

# weighted
wtd.mean(x = df %>% pull(mass), 
         weights = df %>% pull(weights), 
         normwt = TRUE, na.rm = TRUE)
wtd.mean(x = df %>% pull(mass), 
         weights = df %>% pull(weights), 
         normwt = FALSE, na.rm = TRUE)
df %>% mutate(weighted_mass = mass * weights, 
              weights = case_when(is.na(mass) ~ NA_real_, TRUE ~ weights)) %>% 
        summarize(mean = sum(weighted_mass, na.rm = TRUE) / sum(weights, na.rm = TRUE))


###########################


# example with wtd.var, where normwt affects calculation
# https://doc-archives.microstrategy.com/producthelp/10.8/FunctionsRef/Content/FuncRef/WeightedStDev__weighted_standard_deviation_of_a_sa.htm
x <- c(1121694, 209894, 953509, 210152, 2835237, 326485, 229776)
weights <- c(1121694, 209894, 953509, 210152, 2835237, 326485, 229776)

# w normwt = FALSE
sw <- sum(weights)
xbar <- sum(weights * x)/sw
sqrt(sum(weights * ((x - xbar)^2))/(sw - 1))

# w normwt = TRUE
# note that normwt just converts the weights to a percentage of sum(weights), and then multiplies by length(x)
# this ensures the weights sum to length(x) and have mean = 1
weights <- (weights/sum(weights)) * length(x)
sum(weights)
mean(weights)
sw <- sum(weights)
xbar <- sum(weights * x)/sw
sqrt(sum(weights * ((x - xbar)^2))/(sw - 1))



