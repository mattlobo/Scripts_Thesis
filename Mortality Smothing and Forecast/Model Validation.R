# Model Validation

# Forecasting LFPR using the Lee-Carter Method
# Original R code by Bernardo Queiroz

# Function to estimate the parameters of the model
leecarter <- function(nmx){
  log.nmx <- log(nmx)
  ax <- apply(log.nmx, 2, mean)
  swept.mx <- sweep(log.nmx, 2, ax)
  svd.mx <- svd(swept.mx)
  bx <- svd.mx$v[, 1]/sum(svd.mx$v[, 1])
  kt <- svd.mx$d[1] * svd.mx$u[, 1] * sum(svd.mx$v[, 1])
  result <- list(ax = ax, bx = bx, kt = kt)
  return(result)
}

setwd("~/Documents/Monografia/Mortality")
# Loading required packages
library("gdata")
library("reshape2")

# Reading Death counts file
# The file/spreadsheet used is nothing more than the death counts corrected by the completeness factors
# Columns are years and rows are age groups
deaths <- read.xls("Corrected_Data.xlsx", sheet = "Origial_Death_Counts", header = TRUE)
colnames(deaths) <- c("Age", seq(1980, 2010))

# Melting data to reshape it
deaths <- melt(deaths, id.vars = "Age", variable.name = "Year", value.name = "Deaths")

# Reading Population file (Exposures)
# The file/spreadsheet used is nothing more than theinterpolated population for each year 
# Columns are years and rows are age groups
population <- read.xls("Corrected_Data.xlsx", sheet = "Original_Population_Estimates", header = TRUE)
colnames(population) <- c("Age", seq(1980, 2010))

# Melting data to reshape it
population <- melt(population, id.vars = "Age", variable.name = "Year", value.name = "Population")

# Reading data into the Model
D.vec <- deaths$Deaths
E.vec <- population$Population

# Loading package
library(MortalitySmooth)

# Preparing the domains and data in matrices
A <- unique(as.numeric(as.character(deaths$Age)))
Y <- unique(deaths$Year)
D.mat <- matrix(D.vec, length(A), length(Y))
E.mat <- matrix(E.vec, length(A), length(Y))

# Select the data
agemax <- 80
x <- A[A <= agemax]
Y <- as.integer(Y)
y <- Y
D <- D.mat[A <= agemax, ]
E <- E.mat[A <= agemax, ]

# Fit with 2D P-splines
# Default: optimal smoothing parameters selected by BIC
fitPS <- Mort2Dsmooth(x = x, y = y, Z = D, offset = log(E))

# Default plot: shaded contour plot
plot(fitPS)

# Plotting with perspective plot
persp(x, y, fitPS$logmortality, theta = -30,
      col = "red", shade=TRUE, xlab = "Ages (0-80)",
      ylab="Years (1980 - 2010)", zlab = "Mortality rate (log)")

# Plotting deviance residuals
# Histogram
hist(residuals(fitPS), breaks=100)

# Interpolating death rates to each individual age
newages <- seq(0, 80, length = 81)
newdata <- list(x = newages)
pre <- predict(fitPS, newdata = newdata, se.fit = TRUE)

persp(newages, y, pre$fit, theta = -30,
      col="red", shade = 1, border = "black",  xlab = "Ages (0-80)",
      ylab="Years (1980-2010)", zlab="Mortality rate (log)")

rates <- exp(pre$fit)
colnames(rates) <- seq(1980, 2010)
rates <- melt(rates, value.name = "nMx")
colnames(rates) <- c("Age", "Year", "nMx")

# Subsetting the rates files in order to select only the years used in the modelo validation
rates <- subset(rates, Year %in% seq(1980, 1996))
data <- dcast(rates, Age ~ Year, value.var = "nMx")
data <- data[, -1]
nmx <- t(data)

# The data.frame data already has the necessary format years x ages
years <- seq(1980, 1996)
f.years <- seq(1997, 2010)

# Adjusting the Lee-Carter Model for forecasting
model <- leecarter(nmx)

# Removing names from numeric object model$ax
model$ax <- unname(model$ax)

# Data Frame with the parameters
parameters <- data.frame(model$ax, model$bx)
kt <- model$kt

# Preparing kt for forecasting
kt.diff <- diff(kt)
summary.kt <- summary(lm(kt.diff ~ 1))
kt.drift <- summary.kt$coefficients[1,1]
sec <- summary.kt$coefficients[1,2]
see <- summary.kt$sigma

# Actually forecasting kt
h <- seq(0, 13)
kt.stderr <- ( (h*see^2) + (h*sec)^2 )^.5
kt.forecast <- tail(kt, 1) + (h * kt.drift)
kt.lo.forecast <- kt.forecast - (1.96*kt.stderr)
kt.hi.forecast <- kt.forecast + (1.96*kt.stderr)

f.nmx <- matrix(nrow = length(kt.forecast), ncol = length(newages))
for (i in 1:length(kt.forecast)){
  f.nmx[i, ] <- exp((model$bx * kt.forecast[i]) + model$ax)
}

f.nmx <- t(f.nmx)
f.nmx <- as.data.frame(f.nmx)


# Preparing Plot
library(reshape2)
library(ggplot2)

colnames(data) <- years
colnames(f.nmx) <- f.years

row.names(data) <- newages
row.names(f.nmx) <- newages

m.data <- melt(data, value.name = "nMx", variable.name = "Year")
mf.nmx <- melt(f.nmx, value.name = "nMx", variable.name = "Year")

plot.data <- rbind(m.data, mf.nmx)
plot.data$age <- rep(seq(0, 80), each = 1, times = 31)

plot.data.validation <- subset(plot.data, Year %in% c(2010))

qplot(data = plot.data.validation, x = age, y = log(nMx), colour = factor(Year), geom = "line", 
      main = "LOG Death Rtaes Brazil - Selected Year", xlab = "Ages") + facet_wrap(~Year) +
  theme(legend.position = "none")
