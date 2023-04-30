# R Workshop Part 3: Intermediate R 
# Nov 2021
# author: katya
# R version 4.0.2 "Taking Off Again"

# Set working directory to the file location (only RStudio), see workshop 2 if this does not work for you
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Project-oriented workflow highly recommended; all files (code, dependencies, outputs) related to a single project 
# in a designated folder, working directory set to that folder
# browseURL("https://rstats.wtf/project-oriented-workflow.html")

# This function takes a character vector of package names and attempts to load them, or install them if loading fails.
install_load <- function(x){
  for(i in x){
    # require returns TRUE invisibly if it was able to load package
    if(! require(i, character.only = TRUE ) ){
      # If a package could not be loaded then re-install
      install.packages(i, dependencies = TRUE )
      # Load package after installing
      require(i, character.only = TRUE )
    }
  }
}
# Required packages 
install_load(c("reshape2", "stringi", "scales", "purrr", "plyr", "dplyr", "viridis",
               "stringr", "janitor", "data.table", "ggplot2", "randomForest"))

#################################
# Part 1. Advanced data wrangling and an introduction to tidyverse

# install_load("tidyverse")  # or load separately (my preference) ggplot2, dplyr, tidyr, purrr
# browseURL("https://r4ds.had.co.nz/") # free textbook; tidyverse.org 

# 1.1. Useful tricks with dplyr and intro to piping

dat <- data.frame(id = letters[1:10], x = 1:10) # create a dataframe

# Three main ways to handle multiple function calls: nested, intermediate object and piping
# Piping is useful when there is a sequence of transformations of an object and intermediate results are not of interest
dat <- dat %>% # use a piping operator and mutate to create another column change from baseline
  mutate(y = x - first(x))

dat <- dat %>% # use a piping operator and mutate to create another column change from previous
  mutate(z = x - lag(x))

# Mutate with grouping can be very useful especially for large datasets
dat$id2 <- paste(c("spring", "fall")) # create a grouping ID, NB vector is recycled
dat <- dat %>%
  group_by(id2) %>% 
  arrange(x, .by_group = TRUE) %>% 
  mutate(change = x - first(x)) %>% # calculate change from baseline within group
  mutate(sum = cumsum(change)) # calculate cumulative sum within group

# 1.2. reshape:melt
dat2 <- reshape2::melt(dat, value.name = "raw") # convert to loong format

# 1.3. purrr:map split a data frame into pieces, fit a model to each piece, compute summary, extract R2
# This is Hadley's example from tidyverse.org
mtcars %>%  # where did this come from? try >?mtcars
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>% # a function for applying a function to each element of a list (more later)
  map(summary) %>%
  map_dbl("r.squared")

mtcarsSum <- mtcars %>% # will yield deprecated warning
  group_by(cyl) %>% # group by cylinder
  summarize_each(funs(mean(., na.rm=T), n = sum(!is.na(.)), sd = sd(., na.rm = T))) # summarize mean, n obs, standard deviation

# 1.4. tibbles are dataframes with fancier printing options and a few other improvements (maybe)
"keeping what time has proven to be effective, and throwing out what is not"
as.data.frame(mtcars)
as_tibble(mtcars) # immediately shows str(), dim(), head(), useful for big data

# Task 1. Create a new column in mtcars which shows incremental increase in mpg within each cylinder group






#################################
# Part 2. String manipulation and regular expressions: A short introduction

# Most useful for extracting complex IDs, text mining, natural language processing
# Additional resources: 
# browseURL("https://www.hackerearth.com/practice/machine-learning/advanced-techniques/regular-expressions-string-manipulation-r/tutorial/")

# Parsing characters from a string
stri_sub("veryLongIdentifier", 5, -9)

# Removing punctuation
gsub(" ", "", x = "very Long Identifier") # sub spaces by nothing

# Remove all punctuation using posix character classes
gsub(pattern = "[[:punct:]]+",replacement = "", x = "this ID: is a complex; inconsistent... string")

# Greedy vs. non-greedy quantifier
number <- 201000000000100
regmatches(number, gregexpr(pattern = "2.*1",text = "201000000000100")) # matches everything until no ones left
regmatches(number, gregexpr(pattern = "2.?1",text = "201000000000100")) # ? item to its left is optional and is matched at most once

# Extract substring from a complex sequence
string = c("ALS1:E01", "M2:E002", "773R46:E0034") 
# NB different character length before and after the colon, a difficult problem to resolve with stri_sub
gsub(pattern = ".*:",replacement = "", x = string) #".*:" matches everything until it reaches colon

# Extract only the first set of digits in a range
x <- c("75 to 79", "80 - 84", "85 66 89")  # NB how ranges are differently defined
gsub(" .*\\d+", "", x)  # \d matches a digit character, + item to the left is matched one or more times

# A very useful example to keep on hand
a<-" anything goes here, STR1 GET_ME STR2, anything goes here"
gsub(".*STR1 (.+) STR2.*", "\\1", a)

a <- "100001-2020-l_d30_cc" # another complex ID example
gsub(".*-(.+)-.*", "\\1", a) # let's try again - extract year from ID
gsub(".*-", "", a) # extract treatment info

# Task 2. extract first part of the string (c("ALS1:E01", "M2:E002", "773R46:E0034"); 



#################################
# Part 3. Functions and functionals

# 3.1. Basics. A function has 3 parts: formals (list of arguments), body and environment
# browseURL("https://adv-r.hadley.nz/functions.html") # for more info

function_name <- function(arg1, arg2, ...) {
  "Function body" 
}
# What will function_name(x) output? Note an example of lazy evaluation here

f01 <- function(x){
  x + 10
}

f01(4)
f01(seq(1:10))

# Built-in functions: almost everything is done through functions
args(lm)
lm # NB error messages

# 3.2. User-defined functions used to encapsulate repeated actions 
install_load # see our previously defined function, what is x?

# A function to take multiple excel data sheets and write them out as .csv files
excel_to_csv <- function(path, out_dir = NULL) {
  if (is.null(out_dir)) out_dir <- dirname(path)
  sheets <- readxl::excel_sheets(path)
  filenames <- file.path(out_dir, paste0(sheets, ".csv"))
  dats <- lapply(sheets, readxl::read_excel, path = path)
  lapply(seq_along(dats), function(i) readr::write_csv(dats[[i]], filenames[i]))
  invisible()
}
dir.create("files")
excel_to_csv("coef2.xlsx", out_dir = "files")

# Task 3. Identify a repeated action in your workflow and express as a function (homework)

# 3.3. Functionals 
# Functionals take function as an input and return vector as an output

# Can also use the apply family of functions
sapply(mtcars, mean)

map_dbl(mtcars, mean) # here map takes a built-in mean()

# Which one is better? Neither, colNames from base is quicker, better optimized
colMeans(mtcars)

# Can supply a user-defined function
n_unique <- function(x) length(unique(x))
map_dbl(mtcars, n_unique)

map_dbl(mtcars, function(x) length(unique(x))) # and here it takes an anonymous in-line function (same output as above)

# Part of the example we already used above to do linear regression of mpg vs. weight by cylinder group
mtcars %>%  
  split(.$cyl) %>% # group mtcars by cyl
  map(~ lm(mpg ~ wt, data = .)) %>% # use map() functional to apply a function to each element of a list 
  map(summary)

# This split/map example above generates the same result as the following loop
cyls <- unique(mtcars$cyl) # define unique factor levels
for (i in cyls){ 
  dataset <- mtcars[mtcars$cyl == i,]  # Select rows from dataset with particular site label
  fit <- lm(mpg ~ wt, data = dataset) # do a linear regression on each subset
  print(summary(fit)) # print model summary for each cyl group
  # print(coef(summary(fit))[2,4]) # print p-values only
}

# Task 4. Compute the standard deviation of every column in mtcars 


#################################
# # Optional: function factories
# # Graphical factories geom_histogram() example from Advanced R by Hadley Wickham
# # construct sample dataset with different numbers in each cell
# sd <- c(1, 5, 15)
# n <- 100
# 
# df <- data.frame(x = rnorm(3 * n, sd = sd), sd = rep(sd, n))
# 
# ggplot(df, aes(x)) +
#   geom_histogram(binwidth = 2) + 
#   facet_wrap(~ sd, scales = "free_x") +
#   labs(x = NULL)
# 
# # We want to use a function factory that input the desired number of bins 
# # and outputs a function which takes a numeric vector and returns binwidth
# binwidth_bins <- function(n) {
#   force(n)
#   function(x) {
#     (max(x) - min(x))/n
#   }
# }
# 
# ggplot(df, aes(x)) +
#   geom_histogram(binwidth = binwidth_bins(30)) +
#   facet_wrap(~ sd, scales = "free_x") +
#   labs(x = NULL)

#################################
# Part 4. Machine learning, introduction to debugging
# browseURL("https://www.coursera.org/learn/machine-learning") # additional resources for machine learning
# browseURL("https://rstats.wtf/debugging-r-code.html") # additional resources for debugging

# CART classification and regression tree family
install_load(c("rpart", "rpart.plot", "neuralnet")) 
dat <- iris # try >?iris

mod <- rpart(dat$Species ~., data = dat, method ="class")
print(mod)

plot(mod) 
text(mod, use.n = TRUE, all = TRUE, cex = 0.8) # nope. Use rpart.plot to improve rpart plotting

rpart.plot(mod) # rpart.plot improves plotting for rpart

# Random Forests example, normally used for large datasets with many predictors, here in classification mode 
rf <- randomForest(dat$Species ~ ., data = dat[,1:4], ntree = 5000, keep.forest = FALSE,
                   importance = TRUE, na.action = na.roughfix) # na.omit vs. na.roughfix - imputes data, must be MCAR

print(rf) 
varImpPlot(rf, main = "") # built-in plot function, for more attactive figures strip the data and pass on to ggplot
# RF generates thousands of small trees, can output individual trees for visualization but it is not recommended

# Artificial neural networks: allow hidden interaction layers
nn <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Width + Petal.Length, data = dat,
                hidden = 2,linear.output = TRUE)
plot(nn)

# Debugging
# Print debugging and unit test design
labels <-unique(mtcars$cyl)
for (i in labels){
  dataset <- mtcars[mtcars$cyl == i,]  # Select rows from dataset with particular site label
  print("1") # if this step completes successfully, print 1
  ymod<- summary(aov(mpg ~ hp, dat = dataset))   # fit a linear model
  print("2")
  cat(paste('\ncyl:', i, '\n')) # paste number of cylinders
  print(ymod) # paste results
}

# Debug a more complex example
f1 <- function(dat) {
  pdf("rfPlot.pdf", width = 13, height = 9) # write figure to .pdf
  labels <- unique(iris$Species)
  for (i in labels){
    dataset <- iris[iris$Species == i,] # for each species of iris do the next steps
    # dataset <- iris[iris$Species == "setosa",] # debug loop by subsetting to a single factor level
    rf <- randomForest(dataset$Sepal.Length ~ ., data = dataset[,2:4], ntree = 1000, keep.forest = FALSE,
                       importance = TRUE, na.action = na.roughfix)
    print(rf) # debug this step with a simple model eg lm
    varImpPlot(rf, main = paste(dataset$Species, "sepal length variance explained", round(last(rf$rsq)*100, 1), "%"))
    # debug this by removing and gradually building the title
  }
  dev.off()
}

f1(iris)

# Other useful functions: traceback(), trace(), debug() 

#################################
# Part 5. Very brief introduction to improving performance
# Simplify, vectorize, look for alternatives, parallelize, high-performance compute
# Few cookbook solutions, memento unit testing
# install_load(c("microbenchmark", "lubridate", "bench", "parallel"))
# 
# # Compare the speed of apply(x, 1, sum) with rowSums(x) for varying sizes of x
# dimensions <- c(1e0, 1e1, 1e2, 1e3, 0.5e4, 1e4)
# matrices <- lapply(dimensions,function(x) tcrossprod(rnorm(x), rnorm(x))) # what data type is this object?
# 
# bench_rs <- lapply(matrices,function(x) fivenum(microbenchmark(rowSums(x),unit = "ns")$time))
# 
# bench_rs <- data.frame(time = unlist(bench_rs), call = "rowSums", stringsAsFactors = FALSE)
#                      
# bench_apply <- lapply(matrices, function(x) fivenum(microbenchmark(apply(x, 1, sum), unit = "ns")$time))
#                       
# bench_apply <- data.frame(time = unlist(bench_apply), call = "apply", stringsAsFactors = FALSE)
# 
# df <- rbind(bench_rs, bench_apply)
# 
# df$dimension <- rep(dimensions, each = 5)
# df$aggr <- rep(c("min", "lq", "median", "uq", "max"),times = length(dimensions))
# df$aggr_size <- rep(c(1, 2, 3, 2, 1), times = length(dimensions))
# df$group <- paste(as.character(df$call), as.character(df$aggr), sep = " ")
# 
# ggplot(df, aes(x = dimension, y = time, colour = call, group = group)) +
#   geom_point() + 
#   geom_line(aes(linetype = factor(aggr_size, levels = c("3", "2", "1"))), show.legend = FALSE) 
# 
# 
# # lubridate more intuitive syntax comes at a price of lower speed
# bench::mark(
#   as.POSIXct("2013-07-24 23:55:26", tz = "UTC"),
#   ymd_hms("2013-07-24 23:55:26")
# )

# Parallelization
# ncores <- 3
# beginCluster(ncores)
# # function(s)
# endCluster()

#################################
# Part 6. "Advanced" plotting
# Additional resources: https://www.r-graph-gallery.com/ , https://ggplot2.tidyverse.org/ , https://ggplot2-book.org/

# A. 3D and Heatmaps (very useful to avoid overplotting)
x1 <- rnorm(10000,11,.5)
x2 <- rnorm(10000,12,5)
plot(x1, x2) # default cex is 1
plot(x1, x2,  pch = 1, cex = 0.4) # better
col <- colorRampPalette(c("white", "yellow", "green", "blue"), space = "Lab")
smoothScatter(x1, x2, colramp = col) # much better

# B. Scatterplot with histograms, correlograms
install_load(c("ggExtra", "ggcorrplot")) 

data(mpg, package = "ggplot2")

theme_set(theme_bw())  # pre-set the bw theme
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)
ggMarginal(g, type = "histogram", fill="transparent")

# Correlograms
data(mtcars)
corr <- round(cor(mtcars), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("#40E0D0", "white", "purple"), # NB this takes numeric and verbal arguments, mixed
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

# C. Revisit example from part 2
dat <- read.csv("fishFdGl.csv")
p <- ggplot(dat, aes(x = Rich, y = RaoQ, col = FRic)) +  #define aesthetics (x, y)
  geom_point(alpha = 0.7, size = 2) + # define point transparency and size
  # geom_smooth(method = "lm", se = TRUE, size = 0.8, alpha = 0.1, col = "black") + # add a linear fit with SE
  geom_smooth(method = "loess", se = TRUE, size = 0.8, alpha = 0.1, col = "black") + # or a Loess
  xlab("Fish species richness") + ylab("Functional diversity") + # Label axes
  theme_bw() + theme(axis.text.x = element_text(size = 8), axis.title=element_text(size = 14), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = c(0.9, 0.75))  # change theme from default
p + scale_color_viridis(option = "plasma") # add custom color

# D. Bootstrapped function calculates smoothers from 1000 bootstraps followed by calculation of 
# density estimates for each vertical cut visualized by shade intensity
# cite as Schönbrodt, F. 2012. Visually weighted/ Watercolor Plots, new variants. 
# http://www.nicebread.de/visually-weighted-watercolor-plots-new-variants-please-vote/)

source("vwReg.R")
# dat <- read.csv("fishFdGl.csv")
# setEPS()
# postscript("fig1new.eps")
fig1 <- vwReg(FRic ~ Rich, data = dat, shade.alpha = 0, slices = 400, 
              palette=colorRampPalette(c("white", "green", "yellow", "blue"), bias = 5)(20))
fig1 + labs(y = "Functional richness", x="Richness") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
# dev.off()
