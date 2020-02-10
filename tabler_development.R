library(tidyverse)
library(gapminder)


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
source("tabler_helper_functions/add_dummies.R")


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
               size = case_when(row_number == 2 ~ NA_character_, TRUE ~ size)) %>%
        add_dummies(sport)

data_original %>% glimpse()


#############################################################################################


# test tabler
# rows <- c("continent", "era")
rows <- vars(continent, era)
# rows <- "continent"
# cols <- NULL
# cols <- c("size", "life_exp")
# cols <- vars(size, life_exp)
cols <- vars(size, sport.soccer)
# weights <- "weights"
weights <- NULL
stats <- c("n", "pct_col", "pct_row", "pct_all", "valid_pct_col", "valid_pct_row", "valid_pct_all", 
           "n_distinct", "sum", "mean", "median", "mode", "sd", "min", "max", "quantile")
# stats <- c("n", "pct", "valid_pct")
# stats <- NULL
# stat_vars <- NULL
stat_vars <- c("pop", "country", "sport", "gdpPercap")
# stat_vars <- c("country", "year")
# quantiles <- c(0, .25, .5, .75, 1)
quantile <- c(.25, .5, .75, 1)
normalize_weights <- FALSE
na.rm <- TRUE
na.rm_true_stats <- NULL
na.rm_false_stats <- "mean"

data <- data_original
# data <- data %>% mutate(weights = sample(x = seq(from = 1, to = 3, by = .1), size = nrow(data), replace = TRUE))
glimpse(data)

tabler_output <- data %>% tabler(rows = rows, cols = cols, stats = stats, 
                                 stat_vars = stat_vars, quantile = quantile, na.rm = na.rm, 
                                 na.rm_true_stats = na.rm_true_stats, na.rm_false_stats = na.rm_false_stats,
                                 weights = weights, normalize_weights = normalize_weights) 
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
# then use add_dummies on gapminder data, and test implementation of tabler_rows/cols attributes w dummy flags


##############


# prioritized feature list

# add a dummy_var_tbl to attributes
# complete tabler_tests
# add specify_stat_combos argument 
# allow stacking multiple row/col groups crossed against each other

# drop_neg_dummies standalone function 
        # takes a vars arg with tidyselect helpers
        # takes a row/col_groups arg
        # has update_stats arg to specify which stats to update from among totals, pcts, and cumsum
        # e.g. retained cols are asia.1_x_sport.soccer__pct_row and asia.1_x_sport.basketball__pct_row w/ age group row
        # with asia.0_x_sport.soccer__pct_row and asia.0_x_sport.basketball__pct_row negative dummies being dropped
        # do you want asia.1_x_sport.soccer__pct_row/total to remain focused on whole population perspective, or just asia?
# select_rows/cols(); seperate functions for clarity/symmetry; 
        # select by single/multi strings in c(), single bare in c(), single/multi string/bare in vars()
        # full tidyselect helper support in vars()
        # has stats arg defaulting to NULL (includes "totals")
        # has stat_vars arg defaulting to NULL, w/ tidyselect support (negation is supported for string/bare/vars)
        # has row/col_groups arg defaulting to NULL; (eg groups = 1, groups = c(1, 2), groups = 1:3, groups = c(1, 3:4))
        # note any rows/cols specified are returned for all row/col_groups to avoid breaking rectangular shape 
        # will throw warning about not summing to 1 or transparently if part of pct/total row/col vars omitted 
        # has update_stats arg to specify which stats to update from among totals, pcts, and cumsum
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
# add_stats_total standalone functions (can total any stat, even if it's weird like sd)
        # eg var.value_x_var.value__stat.stat_vars__total, var.value_x_var.value__stat.stat_vars__cumsum
        # it only makes sense to pass stats args (and the across_groups arg)
        # for multiple col groups, totals can recognize col groups by attribute tbls
        # the total cols can then be added as "total_col_group_1" etc and inserted after col
        # or if the argument is passed for across_groups = TRUE, a single "total_all_col_groups" can be added
# add_stats_cumsum; takes same target_cols, stats, stat_vars, col_group args as specify_stat_combos
        # cumsum should be done after tabler because it gives chance to drop/reorder rows w/o messing up cumsum
# add_row_total
        # it only makes sense to pass stats args (and the across_groups arg)
        # for multiple row groups, totals can recognize row groups by attribute tbls
        # the total row can then be added as "total_row_group_1" etc and inserted after row
        # or if the argument is passed for across_groups = TRUE, a single "total_all_row_groups" can be added
# reorder_rows/cols; order stat_cols based on vars, stats, stat_vars, groups, or specify_vars for full control
        # has across_groups = FALSE default arg
        # reorder_cols can pass multiple criteria; eg by = c("groups", desc("stat_vars"), "stats", "vars")
        # reorder_rows can only take by = "vars" or "groups"
        # will flag warning if reorder_rows called when stat_cumsum_col is present
        # has update_stats arg to specify which stats to update from among totals, pcts, and cumsum (cumsum only concern)
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



