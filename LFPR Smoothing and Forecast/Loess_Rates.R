# Import file with the rates, years are columns and ages (starting from 10 yrs old) are rows
# You need a file with the LFPR calculated for Single-Age groups.
# Read the file from your working directory.
# setwd("~/Documents/Monografia/ELRP/Data")
# data <- read.csv("~/Documents/Monografia/ELRP/Data/original.rates.csv", dec = ".", header = F)

# Create a vector with all the years you have available
# In this case data is not available for 1980, 1991, 1994, 2000, and 2010
years <- c(1979, seq(1981, 1990, 1), 1992, 1993, seq(1995, 1999, 1), seq(2001, 2009, 1), 2011, 2012, 2013)

# Create a vector with all the ages (10 - 80)
ages <- c(seq(10, 80, 1))

# Function to do LOESS smoothing, the smoothing parameter is selected by AIC_C
# by Kyle Gorman <gormanky@ohsu.edu>

aicc.loess <- function(fit) {
  # compute AIC_C for a LOESS fit, from:
  # 
  # Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing 
  # parameter selection in nonparametric regression using an improved 
  # Akaike Information Criterion. Journal of the Royal Statistical 
  # Society B 60: 271–293.
  # 
  # @param fit        loess fit
  # @return           'aicc' value
  stopifnot(inherits(fit, 'loess'))
  # parameters
  n <- fit$n
  trace <- fit$trace.hat
  sigma2 <- sum(resid(fit) ^ 2) / (n - 1)
  return(log(sigma2) + 1 + (2 * (trace + 1)) / (n - trace - 2))
}


autoloess <- function(fit, span=c(.1, .9)) {
  # compute loess fit which has span minimizes AIC_C
  # 
  # @param fit        loess fit; span parameter value doesn't matter
  # @param span       a two-value vector representing the minimum and 
  #                   maximum span values
  # @return           loess fit with span minimizing the AIC_C function
  stopifnot(inherits(fit, 'loess'), length(span) == 2)
  # loss function in form to be used by optimize
  f <- function(span) aicc.loess(update(fit, span=span))
  # find best loess according to loss function
  return(update(fit, span=optimize(f, span)$minimum))
}

# For loop thta puts all the data in a matrix with the lowess smoothing method
rates.loess <- matrix(nrow = length(ages), ncol = length(years))
for (i in 1:length(years)){
  rates.loess[, i] <- autoloess(loess(data[, i] ~ ages))$fitted
}

# Creates a data frame from the original matrix and saves it as a csv file
new.rates <- as.data.frame(rates.loess, row.names = NULL)
colnames(new.rates) <- years

# Write a csv file with your smoothed rates
# write.csv(x = new.rates, file = "new.rates.csv")


