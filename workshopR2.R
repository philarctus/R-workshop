# Set working directory to the file location (only RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This function will take a character vector of package names and attempt to load them, or install them if loading fails.
install_load <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}
# Required packages
install_load(c("reshape2", "stringi", "scales", "stringr", "plyr", "dplyr", "viridis", "readxl", "ggplot2", "vegan"))

# Load the file 
dat <- read.csv("fishFdGl.csv") # can use any name for your object but it cannot start with a number

# Examine object structure, note factors, characters, numeric, are dates read in correctly
str(dat)

# Examine data summary, note if any variables have unexpected NAs (will affect behavior of some functions
summary(dat)

# Subset the data to exclude certain values (eg < 0, NA, certain sites)
dat <- subset(dat, RaoQ > 0) 

###################################
# Part 2. Regression (and its non-linear flavors, transformations, custom figures and intro to debugging)
# Simple linear regression
fit <- lm(Rich ~ FRic, data = dat)
summary(fit) # summarize model output

# LR has stringent assumptions, check if those are met
plot(fit) # plot is an extremely versatile function, performs differently depending on package and object!

# Plot the relationship, from base and ggplot
plot(Rich ~ FRic, data = dat)
plot(dat$Rich ~ dat$FRic) # an alternative 
plot(Rich ~ FRic) # this won't work unless you define those objects beforehand!

# Log-transformation can be added directly to the function
plot(Rich ~ log(FRic), data = dat) # note that unlike Excel default log is base e not 10

# Multiple Linear Regression (and object re-assignment)
fit <- lm(Rich ~ FRic + RaoQ, data = dat)
summary(fit) # NB this over-wrote the previous "fit" object, it is irretrievably lost


###################################
# An aside: most complex graphics can be build from scratch in base, but ggplot is more common
p <- ggplot(dat, aes(x = Rich, y = RaoQ, col = FRic)) +  #define aesthetics (x, y)
  geom_point(alpha = 0.7, size = 2) + # define point transparency and size
  # geom_smooth(method = "lm", se = TRUE, size = 0.8, alpha = 0.1, col = "black") + # add a linear fit with SE
  geom_smooth(method = "loess", se = TRUE, size = 0.8, alpha = 0.1, col = "black") + # or a Loess
  xlab("Fish species richness") + ylab("Functional diversity") + # Label axes
  theme_bw() + theme(axis.text.x = element_text(size = 8), axis.title=element_text(size = 14), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = "NONE")  # change theme from default
p + scale_color_viridis(option = "plasma") # add custom color

# Let's test this code step by step to understand code testing/debugging
ggplot(dat, aes(x = Rich, y = RaoQ, col = FRic))
# Empty plot: we did not specify what we want plotted, histograms, points 

ggplot(dat, aes(x = Rich, y = RaoQ, col = FRic)) +  
  geom_point(alpha = 0.7, size = 2) # NB remove the trailing + when testing step by step

ggplot(dat, aes(x = Rich, y = RaoQ, col = FRic)) +
  geom_point(alpha = 0.7, size = 2) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.8, alpha = 0.1, col = "black") 


###################################
# Part 3. ANOVA (plus joining, creating columns and parsing names)
# Predictors are in a different source file, read it in and join (one of the most useful functions for ex-Excel users)

pred <- read.csv("fishFdGlFactorsFake.csv") #filename means these are made-up predictors
full <- join(dat, pred, by = "Site", type = "left") # joining column (by) name must be exactly equivalent 
#in this example, left and full will give identical results, not so if one dataset is shorter

str(full)
# One-way ANOVA
fit <- aov(Rich ~ lake, dat = full)
summary(fit)
TukeyHSD(fit)
# Two-way ANOVA
fit <- aov(Rich ~ lake + TP + lake:TP, dat = full)
summary(fit)
# ANCoVA
fit <- aov(Rich ~ lake + FRic, dat = full)
summary(fit)

# An aside: learn to create new columns, parse and create new labels
full$id <- paste(full$lake, full$TP, sep = ".") # create a new ID from lake and TP, separated by .
# on a second though, I only want the first part of lake's string
full$id <- paste(stri_sub(full$lake, 1,2), full$TP, sep = ".") 

fit <- aov(Rich ~ id, dat = full)
summary(fit)


###################################
# Part 4. Introduction to ordination 
# Most commonly used to summarize species or environmental data in fewer dimensions
# Ordinations in vegan
dat2 <- read.csv("species.csv")
spp <- dat2[,2:66] #subset dataframe to exclude ID and predictors
env <- dat2[,67:69] #subset dataframe to environmental predictors
# Remove rare species
spp2 <- spp[ ,colSums(spp != 0) > 5] # literally: subset dataframe to columns whose sums of values not equal to zero is more than 5

# Non-metric Multidimensional scaling
fit <- metaMDS(spp2) 
fit

# Note there is no convergence, do not proceed
# Let's use this as an example to learn default arguments of a function
??metaMDS

# scroll to usage, note argument defaults distance = "bray", k (dimensions) = 2, trymax = 20 (number of random tries to reach solution)

fit <- metaMDS(spp2, k = 3, maxtry = 40, autotransform = FALSE) 
fit
# see more examples at https://cran.r-project.org/web/packages/vegan/vignettes/FAQ-vegan.html

plot(fit)
plot(fit, display = "species", type = "text") #useful to check outliers


# Let's bring in treatment/predictor variables
trt <- env[,2]

col <- viridis(3, direction = -1)
# col <- c("purple", "blue", "green", "red", "orange")
plot(fit, display = "sites")
points(fit, col = col[trt], bg = col[trt], pch = 21)
legend("bottomright", legend = levels(trt), col = col, pt.bg = col, bty = "n", pch = 21)
ordihull(fit, groups = trt, draw ="polygon", col = col, label = FALSE)

###################################
# Part 5. Most common R error codes
# Note that running this will result in errors. That is the point of this exercise.

# could not find function: whew, package not loaded, or misspelled
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
x <- c(1:5, "a")
str(x)

# Function-specific error codes
# only defined on a data frame with all numeric variables 
metaMDS(dat2) #tried to run NMDS on a full dataset with IDs
