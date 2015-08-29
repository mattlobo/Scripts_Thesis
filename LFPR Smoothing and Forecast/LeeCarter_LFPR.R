# Forecasting LFPR using the Lee-Carter Method
# Original R code by Bernardo Queiroz

# Function to estimate the parameters of the model
leecarter <- function(lfpr){
	log.lfpr <- log(lfpr)
	ax <- apply(log.lfpr, 2, mean)
	swept.lfpr <- sweep(log.lfpr, 2, ax)
	svd.lfpr <- svd(swept.lfpr)
	bx <- svd.lfpr$v[ , 1]/sum(svd.lfpr$v[ , 1])
	kt <- svd.lfpr$d[1]*svd.lfpr$u[ , 1]*sum(svd.lfpr$v[ , 1])
	result <- list(ax = ax, bx = bx, kt = kt)
	return(result)
}

# Importing file and making sure it is the required format ages x years, the file is called interpolated_rates
setwd("~/Documents/Monografia/Labor Rates/")
data <- read.csv("interpolated_rates.csv", header = F)
data <- data[ , -1]
years <- seq(1980, 2013)
f.years <- seq(2014, 2025)
ages <- seq(10, 80)

# Only transpose if the data is in the format years x ages
lfpr <- t(data)

# Fitting the Lee-Carter model
model <- leecarter(lfpr)

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
h <- seq(0, 11)
kt.stderr <- ( (h*see^2) + (h*sec)^2 )^.5
kt.forecast <- tail(kt, 1) + (h * kt.drift)
kt.lo.forecast <- kt.forecast - (1.96*kt.stderr)
kt.hi.forecast <- kt.forecast + (1.96*kt.stderr)

f.lfpr <- matrix(nrow = length(kt.forecast), ncol = length(ages))
for (i in 1:length(kt.forecast)){
	f.lfpr[i, ] <- exp((model$bx * kt.forecast[i]) + model$ax)
}

f.lfpr <- t(f.lfpr)
f.lfpr <- as.data.frame(f.lfpr)

# write.csv(f.lfpr, "forecast_rates.csv")

# Preparing Plot
library(reshape2)
library(ggplot2)

colnames(data) <- years
colnames(f.lfpr) <- f.years

row.names(data) <- ages
row.names(f.lfpr) <- ages

m.data <- melt(data, value.name = "LFPR", variable.name = "Year")
mf.lfpr <- melt(f.lfpr, value.name = "LFPR", variable.name = "Year")

plot.data <- rbind(m.data, mf.lfpr)
plot.data$age <- rep(seq(10, 80), each = 1, times = 46)

plot.data.select <- subset(plot.data, Year %in% c(1980, 1991, 2000, 2010, 2015, 2020, 2025))

qplot(data = plot.data.select, x = age, y = LFPR, colour = factor(Year), geom = "line", 
      main = "LFPR Brazil - Selected Years") + facet_wrap(~Year) + theme(legend.position = "none")

qplot(data = plot.data.select, x = age, y = LFPR, colour = factor(Year), geom = "line", 
      main = "LFPR Brazil - Selected Years")

qplot(data = plot.data, x = age, y = LFPR, colour = factor(Year), geom = "line", 
      main = "LFPR Brazil") + guides(col = guide_legend(ncol = 4))

parameters$age <- seq(10, 80)
qplot(data = parameters, x = age, y = model.ax, geom = "line")

qplot(data = parameters, x = age, y = model.bx, geom = "line")

plot.kt <- kt
plot.kt <- as.data.frame(plot.kt)
plot.kt$years <- seq(1980, 2013)
qplot(data = plot.kt, x = years, y = kt, geom = "line")
