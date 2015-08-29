# Defining work directory
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

# write.csv(exp(pre$fit), "Single_Age_Rates.csv")

# Projecting Death Rates for 2011 - 2025

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

# The file used as input in the function is the rates data.frame generated above

data <- dcast(rates, Age ~ Year, value.var = "nMx")
data <- data[, -1]
nmx <- t(data)

# The data.frame data already has the necessary format years x ages
years <- seq(1980, 2010)
f.years <- seq(2011, 2025)

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
h <- seq(0, 14)
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
plot.data$age <- rep(seq(0, 80), each = 1, times = 46)

plot.data.select <- subset(plot.data, Year %in% c(1980, 1991, 2000, 2010, 2015, 2020, 2025))

qplot(data = plot.data.select, x = age, y = nMx, colour = factor(Year), geom = "line", 
      main = "Death Rates Brazil - Selected Years", xlab = "Ages", ylab = "Death Rates") + 
  facet_wrap(~Year) + theme(legend.position = "none")

qplot(data = plot.data.select, x = age, y = nMx, colour = factor(Year), geom = "line", 
      main = "Death Rates Brazil - Selected Years", xlab = "Ages", ylab = "Death Rates") + theme(legend.title = 
                                                              element_text(size = 12, face = "bold")) +
  scale_color_discrete(name = "Year")

qplot(data = plot.data, x = age, y = nMx, colour = factor(Year), geom = "line", 
      main = "Death Rates Brazil", xlab = "Ages", ylab = "Death Rates") + 
  theme(legend.title = element_text(size = 12, face = "bold")) +
  scale_color_discrete(name = "Year") + guides(col = guide_legend(ncol = 4))

parameters$age <- seq(0, 80)
qplot(data = parameters, x = age, y = model.ax, geom = "line")

qplot(data = parameters, x = age, y = model.bx, geom = "line")

plot.kt <- kt
plot.kt <- as.data.frame(plot.kt)
plot.kt$years <- seq(1980, 2010)
qplot(data = plot.kt, x = years, y = kt, geom = "line")





# Creating Life Tables for all Census years (1980 - 2010)

life.table <- function( x, nMx){
  # simple lifetable using Keyfitz and Flieger separation factors and 
  # exponential tail of death distribution (to close out life table)
  b0 <- 0.07;   b1<- 1.7;      
  nmax <- length(x)
  #nMx = nDx/nKx   
  n <- c(diff(x), 999)          		  # width of the intervals
  nax <- n/2;		            	        # default to .5 of interval
  nax[1] <- b0 + b1 * nMx[1]    		  # from Keyfitz & Flieger(1968)
  nax[nmax] <- 1/nMx[nmax] 	  	      # e_x at open age interval
  nqx <- (n * nMx) / (1 + (n - nax) * nMx)
  nqx<-ifelse(nqx > 1, 1, nqx);		    # necessary for high nMx
  nqx[nmax] <- 1.0
  lx <- c(1, cumprod(1 - nqx));   	  # survivorship lx
  lx <- lx[1:length(nMx)]
  ndx <- lx * nqx;
  nLx <- n * lx - nax * ndx;      	 # equivalent to n*l(x+n) + (n-nax)*ndx
  nLx[nmax] <- lx[nmax] * nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax], NA);
  lt <- data.frame(Ages = x, nqx = nqx, lx = lx, ndx = ndx, nLx = nLx, Tx = Tx, ex = ex, nMx = nMx)
  return(lt)
}

Census_1 <- subset(rates, Year %in% 1980)
LT1 <- life.table(newages, Census_1$nMx)

Census_2 <- subset(rates, Year %in% 1991)
LT2 <- life.table(newages, Census_2$nMx)

Census_3 <- subset(rates, Year %in% 2000)
LT3 <- life.table(newages, Census_3$nMx)

Census_4 <- subset(rates, Year %in% 2010)
LT4 <- life.table(newages, Census_4$nMx)

Projection_1 <- subset(mf.nmx, Year %in% 2015)
LT5 <- life.table(newages, Projection_1$nMx)

Projection_2 <- subset(mf.nmx, Year %in% 2020)
LT6 <- life.table(newages, Projection_2$nMx)

Projection_3 <- subset(mf.nmx, Year %in% 2025)
LT7 <- life.table(newages, Projection_3$nMx)

names <- list("LT1", "LT2", "LT3", "LT4", "LT5", "LT6", "LT7")

library(WriteXLS)
WriteXLS(names, "LifeTables.xlsx")
