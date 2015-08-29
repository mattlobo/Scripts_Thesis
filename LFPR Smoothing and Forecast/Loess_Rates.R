# Import file with the rates, years are columns and ages (starting from 10 yrs old) are rows
data <- read.csv("~/Documents/Monografia/ELRP/Data/original.rates.csv", dec = ".", header = F)
setwd("~/Documents/Monografia/ELRP/Data")

# Create a vector with all the years you have available
years <- c(1979, seq(1981, 1990, 1), 1992, 1993, seq(1995, 1999, 1), seq(2001, 2009, 1), 2011, 2012, 2013)

# Create a vector with all the ages (10 - 80)
ages <- c(seq(10, 80, 1))

# For loop thta puts all the data in a matrix with the lowess smoothing method
rates.loess <- matrix(nrow = length(ages), ncol = length(years))
for (i in 1:length(years)){
  rates.loess[, i] <- autoloess(loess(data[, i] ~ ages))$fitted
}

# Creates a data frame from the original matrix and saves it as a csv file
new.rates <- as.data.frame(rates.loess, row.names = NULL)
colnames(new.rates) <- years
write.csv(x = new.rates, file = "new.rates.csv")


