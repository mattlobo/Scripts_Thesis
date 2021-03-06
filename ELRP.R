########################################################
########IMPORTANT: 81 ARE AGES AND 46 ARE YEARS#########
###ALSO, SOMETIMES I REMOVED ROWS BELOW THE AGE OF 50###
########################################################

# Setting work directory
setwd("~/Documents/Monografia/Labor Rates/")

# Generating the column names
or.years <- seq(1980, 2013)
f.years <- seq(2014, 2025)

# Reading and adjusting the data to our needs
or.lfpr <- read.csv("interpolated_rates.csv")
or.lfpr <- or.lfpr[, -1]
f.lfpr <- read.csv("forecast_rates.csv")
f.lfpr <- f.lfpr[, -1]

# Combining the data
lfpr <- cbind(or.lfpr, f.lfpr)

# Study 1 - 1980 lfpr

# lfpr <- rep(or.lfpr[, 1], times = 46)
# lfpr <- matrix(lfpr, ncol = 46)

# Renanimng columns
colnames(lfpr) <- c(or.years, f.years)

# We only need LFPR data starting at age 49, so let's get rid of the rest !!!
lfpr <- lfpr[-seq(1,39), ]

# Calculating Rx (the rate of Retirement)
rx <- 1-lfpr
#rx <- rx[-1,]

# Calculating Tx (probability of remaining in the labor force)
tx <- 1-rx

# Calculating gamma (probability of retiering at age x conditional to survival until age x)
gamma <- matrix(0, 31, 46)

for (j in 1:ncol(gamma)){
  for (i in 1:nrow(gamma)){
    gamma[i,j] <- 1-((1-rx[i+1,j])/(1-rx[i,j]))
  }
}

setwd("~/Documents/Monografia/Mortality/")

# Function to calculate Life Tables
life.table <- function(x, nMx){
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

# Function to get the life expectancy of a newborn
get.lt <- function(x){
  return(life.table(newages, x))
}

# Read nmx files to generate yearly life tables
# or.nmx <- read.csv("Single_Age_Rates.csv", header = T)
# or.nmx <- or.nmx[, -1]
# f.nmx <- read.csv("f.nmx.csv", header = T)
# f.nmx <- f.nmx[, -1]
# nmx <- cbind(or.nmx, f.nmx)

# Study 2 - nmx 1980
nmx <- rep(or.nmx[, 1], times = 46)
nmx <- matrix(nmx, ncol = 46)

colnames(nmx) <- c(or.years, f.years)

# We only need nmx starting at age 20...
# nmx <- nmx[-seq(1, 20), ]
newages <- seq(0, 80)

# Get lx for all years and ages
lx <- matrix(0, 81, 46)

for (i in 1:ncol(lx)){
  lx[, i] <- get.lt(nmx[, i])$lx
}

dimnames(lx) <- list(newages, seq(1980, 2025))

# Get qx for all years and ages
nqx <- matrix(0, 81, 46)

for (i in 1:ncol(nqx)){
  nqx[, i] <- get.lt(nmx[, i])$nqx
}

dimnames(nqx) <- list(newages, seq(1980, 2025))

# Get ex for all years and ages
ex <- matrix(0, 81, 46)

for (i in 1:ncol(ex)){
  ex[, i] <- get.lt(nmx[, i])$ex
}

dimnames(ex) <- list(newages, seq(1980, 2025))


# Get Sx for all years and ages (probability of remaining alive among those who survive to age x)
sx <- matrix(0, 81, 46)

sx[seq(1, 51), ] <- 1
for (j in 1:ncol(sx)){
  for (i in 51:nrow(sx)){
    sx[i, j] <- lx[i, j]/lx[51, j]
  }
}

dimnames(sx) <- list(newages, seq(1980, 2025))

# Get rho for all years (probability of surviving until age 50 at age 20)
rho <- c()

for (i in 1:46){
  rho[i] <- lx[51, i]/lx[21, i]
}

# Now we will eliminate unecessary information sx, nqx and ex
sx <- sx[-seq(1,50), ]
nqx <- nqx[-seq(1,50), ]
ex <- ex[-seq(1,50), ]

# Now calculate the average life expectancy
avg.ex <- matrix(0, 31, 46)
for (i in 1:nrow(avg.ex)-1){
  avg.ex[i, ] <-  (ex[i, ]+ex[i+1, ])/2 
}
avg.ex[31, ] <- ex[31, ]/2

# ELRP calculation

prev.elrp <- matrix(0, 31, 46)

for (j in 1:ncol(prev.elrp)){
  for(i in 1:nrow(prev.elrp)){
    prev.elrp[i, j] <- sx[i, j]*gamma[i, j]*tx[i+1, j]*(1-0.5*nqx[i, j])*avg.ex[i, j]
  }
}

ELRP <- apply(prev.elrp, 2, FUN = sum)*rho


# Get e20 for all years

get.e20 <- function(x){
  return(life.table(newages, x)$ex[21])
}

e20 <- c()

for (i in 1:46){
  e20[i] <- get.e20(nmx[, i])
}


plot.ELRP <- (ELRP/e20)*100
plot.ELRP <- data.frame(plot.ELRP)
plot.ELRP$years <- seq(1980, 2025) 

plot.e20 <- data.frame(e20)
plot.e20$years <- seq(1980, 2025) 

# study.0 <- plot.ELRP
# study.1 <- plot.ELRP
study.2 <- plot.ELRP

final.plot <- rbind(study.0, study.1, study.2)
final.plot$type <- rep(c("Baseline Scenario", "1980 LFPR Scenario", "1980 Death Rates Scenario"), each = 46)

write.csv(final.plot, "Baseline+Scenarios.csv")

library(ggplot2)

qplot(data = final.plot, x = years, y = plot.ELRP, colour = type,
      geom = c("point", "path"), ylab = "ELRP / Life Expectancy at age 20", xlab = "Years") + theme(legend.position = "bottom") +
                                                                                                  labs(color = "")

