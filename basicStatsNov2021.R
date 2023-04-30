# R Workshop Part 2: Basic statistics in R made simple
# Date: 10/27/2021
# author: katya
# R version 4.0.2 "Taking Off Again"

###################################
# Part 1. Configure the environment
# Set working directory (where the source files are) to the file location (only RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# If the above does not work (and the folder has been unzipped),
# manually set working directory by setwd("~/your path"), or, if you don't know the path,
# by going to top menu -> Session -> Set Working Directory -> Choose Directory

# Install and read in required packages
install.packages(c("reshape2", "stringi", "scales", "stringr", "plyr", "dplyr", "viridis", "ggplot2", "vegan", "Hmisc", "Rcpp"))
library(reshape2) # data wrangling
library(stringi) # string manipulation
library(stringr) # string manipulation
library(scales) # plotting
library(plyr) # data wrangling
library(dplyr) # data wrangling
library(viridis) # color palette
library(ggplot2) # plotting
library(vegan) # ordination
library(Hmisc) # to generate cross-correlations with p-values
library(Rcpp)

# Load the file, data from doi/10.1086/704713
dat <- read.csv("fishFdGl.csv") # can use any name for your object but it cannot start with a number

# Examine object structure, note factors, characters, numeric, ensure that dates are read in correct format
# Most error codes in the beginning will result from incorrect data structure, see handout at
# browseURL("https://library.stanford.edu/research/data-management-services/case-studies/case-study-spreadsheets")
str(dat)
str(iris) # some datasets are already provided in R, useful for testing

# Examine data summary, note if any variables have unexpected NAs (will affect behavior of some functions)
summary(dat)

# Subset the data to exclude certain values (eg < 0, NA, certain sites)
dat <- subset(dat, RaoQ > 0) 

###################################
# Part 2. Regression (and its non-linear flavors, transformations, custom figures and intro to debugging)

# Simple linear regression
fit <- lm(Rich ~ FRic, data = dat)
summary(fit) # summarize model output

# LR has stringent assumptions, check if those are met
plot(fit) # plot is a generic function, performs differently depending on package and object type!

# Simple plot of the relationship, from base and ggplot
plot(Rich ~ FRic, data = dat)
plot(dat$Rich ~ dat$FRic) # an alternative 
plot(Rich ~ FRic) # this won't work unless you define those objects beforehand!

# Log-transformation can be added directly to the function
plot(Rich ~ log(FRic), data = dat) # note that, unlike Excel, default log is base e not 10

# Output results of cross-correlations with p-values as a .csv
allCoef <- rcorr(as.matrix(dat[,-1]), type = "pearson") # can also do spearman; removing site IDs from dat
write.csv(unclass(allCoef), "corr.csv") # unclass a list to enable conversion to .csv; here's your list btw

# Multiple Linear Regression (and object re-assignment)
fit <- lm(Rich ~ FRic + RaoQ, data = dat)
summary(fit) # NB this over-wrote the previous "fit" object, it is irretrievably lost

# Task 1. Test the linear relationship between Sepal.Width and Petal.Width in the iris data



###################################
# More advanced plotting
pdf("richnessRaoQ.pdf", width = 6, height = 4) # use to write to a .pdf, don't forget to close off the device
p <- ggplot(dat, aes(x = Rich, y = RaoQ, col = FRic)) +  #define aesthetics (x, y)
  geom_point(alpha = 0.7, size = 2) + # define point transparency and size
  geom_smooth(method = "loess", se = TRUE, size = 0.8, alpha = 0.1, col = "black") + # model type, SE transparency, color
  xlab("Fish species richness") + ylab("Functional diversity") + # Label axes
  theme_bw() + theme(axis.text.x = element_text(size = 8), axis.title=element_text(size = 14), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = c(0.9, 0.75))  # change theme from default
p + scale_color_viridis(option = "plasma") # add custom color
dev.off() # close off the device if outputting to .pdf; if file is open will give an over-write error

# Let's test this code step by step to understand code testing/debugging
ggplot(dat, aes(x = Rich, y = RaoQ, col = FRic)) # Empty plot: did not specify what we want plotted, histograms, points 

ggplot(dat, aes(x = Rich, y = RaoQ, col = FRic)) +  
  geom_point(alpha = 0.7, size = 2) # NB remove the trailing + when testing step by step

ggplot(dat, aes(x = Rich, y = RaoQ, col = FRic)) +
  geom_point(alpha = 0.7, size = 2) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.8, alpha = 0.1, col = "black") 

show_col(viridis_pal()(10))
show_col(viridis_pal(option = "inferno")(20))


###################################
# Part 3. ANOVA (plus joining, creating columns and parsing names, loops)

# Predictors are in a different source file, read it in and join (one of the most useful for ex-Excel users)
pred <- read.csv("fishFdGlFactorsFake.csv") # filename means these are made-up predictors
full <- join(dat, pred, by = "Site", type = "left") # joining column name must be exactly equivalent 

# One-way ANOVA with Tukey pairwise comparisons
fit <- aov(Rich ~ lake, dat = full)
summary(fit)
TukeyHSD(fit)

# Two-way ANOVA with interaction effect
fit <- aov(Rich ~ lake + TP + lake:TP, dat = full)
summary(fit) # NB fit is over-written

# ANCoVA
fit <- aov(Rich ~ lake + FRic, dat = full)
summary(fit)

# An aside: learn to create new columns, parse and create new labels
full$id <- paste(full$lake, full$TP, sep = ".") # create a new ID from lake and TP, separated by .

# on a second though, I only want the first part of lake's string, and as factor, not as character
full$id <- as.factor(paste(stri_sub(full$lake, 1,2), full$TP, sep = "."))
fit <- aov(Rich ~ id, dat = full)
summary(fit)

# Create a quantitative column based on a factor via ifelse statement
full$concentration<- ifelse(full$TP == "low", 0.5, 3)
summary(full$concentration)

### ANOVA for each factor level, loops and saving output to text file

# sink('analysis-output.txt', append=TRUE) # this will output results directly to file
labels <-unique(full$lake)
for (i in labels){
  dataset <- full[full$lake == i,]  # Select rows from dataset with particular site label
  ymod<- summary(aov(Rich ~ TP, dat = dataset))   # Fit a one-way ANOVA
  cat(paste('\nLake:', i, '\n')) # paste lake name
  print(ymod) # paste results
}
# sink() # forget to close the sink and everything else you do will append to that file no error flag!

# Task 2. Test an ANOVA model of Sepal.Width by Species in the iris data followed by the Tukey pairwise comparison




###################################
# Part 4. Introduction to ordination 
# Most commonly used to summarize species or environmental data in fewer dimensions

# read in the dataset
dat2 <- read.csv("species.csv")
spp <- dat2[,2:66] #subset dataframe to exclude ID and predictors
env <- dat2[,67:69] #subset dataframe to environmental predictors

# Remove rare species, ordination is often sensitive to very rare species
spp2 <- spp[ ,colSums(spp != 0) > 5] # subset dataframe to columns whose sums of values not equal to zero > 5

# Non-metric Multidimensional scaling
fit <- metaMDS(spp2) 
fit

# Note there is no convergence, do not proceed
# Let's use this as an example to learn default arguments of a function
??metaMDS

# scroll to usage, note argument defaults distance = "bray", k (dimensions) = 2, 
# trymax = 20 (number of random tries to reach solution)
fit <- metaMDS(spp2, k = 3, maxtry = 40, autotransform = FALSE) 
fit
# browseURL("https://cran.r-project.org/web/packages/vegan/vignettes/FAQ-vegan.html") # more examples 

# Simple plot of ordination results in 2D
plot(fit)
plot(fit, display = "species", type = "text") #useful to check outliers

# Let's bring in treatment/predictor variables and do a more complex
trt <- env[,2]

col <- viridis(3, direction = -1) # define color scheme
# col <- c("purple", "blue", "green", "red", "orange") # alternative color scheme

plot(fit, display = "sites")
points(fit, col = col[trt], bg = col[trt], pch = 21)
legend("bottomright", legend = levels(as.factor(trt)), col = col, pt.bg = col, bty = "n", pch = 21)
ordihull(fit, groups = trt, draw ="polygon", col = col, label = FALSE)

# Task 3. Run a Principal Component Analysis on iris data. Tips: can use rda() in vegan; remove non-numeric Species col



###################################
# Part 5. Most common R error codes

# could not find function: whew, package not loaded
# object not found: object has not been created, likely misspelled object name
plot(ft, display = "sites")

# unexpected symbol or input: incorrect punctuation
# plot(fit;  display = "sites")
# plot(fit  display = "sites")

# subscript out of bounds: 
# variable lengths differ: most often because comparing vectors of unequal size
x <- seq(1, 10, 1)
y <- seq(1, 10, 0.5)
plot(x ~ y)

# object of type 'closure' is not subsettable
# non-numeric argument to a binary operator: check data structure but could be much more complex
# A single non-numeric character makes the entire column a character, watch for N/A vs NA
x <- c(1:5, "a")
str(x)
x <- c(1:5, NA)
str(x)

# Function-specific error codes
# only defined on a data frame with all numeric variables 
metaMDS(iris) #tried to run NMDS on a full dataset with IDs

###################################
# Next steps: practice R coding and basic stats at your own pace with 
# install.packages("swirl")
# library(swirl)
# swirl()

# Read ggplot textbook https://ggplot2-book.org/ for further insight on plotting
# Use https://stackoverflow.com/ to get answers or search top rated questions for peer-reviewed statistical advice
