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

# Loading Packages
library(reshape2)

# Read the original file with rates estimated for the PNAD years
data <- read.csv("smoothed_rates.csv", header = T)
data <- data[ , -1]

# The years vector contains all the years (even the ones you're abaout to interpolate)
years <- (seq(1979, 2013))

# Create vector with the years available  and call it original_years
or.years <- c(1979, seq(1981, 1990, 1), 1992, 1993, seq(1995, 1999, 1), seq(2001, 2008, 1), 2011, 2012, 2013)

colnames(data) <- or.years

# Creates the Ages column in the data.frame original_rates
data$Ages <- seq(10, 80)

# Use the melt function from the reshape2 package to melt the original rates file
m.rates <- melt(data, id.vars = "Ages", variable.name = "Years", value.name = "LFPR")

# The new rates file still has only the original rates for the PNAD years (no interpolation here)
new_rates <- dcast(data = m.rates, formula = Years~Ages, value.var = "LFPR")

# Remove Years column, this was only used as a reference for the melt/dcast functions
new_rates <- new_rates[ , -1]

# Converts the new_rates data.frame to a Matrix (important when dealing with the interppolation, but 'll check later)
new_rates <- data.matrix(new_rates)

# Creates a matrix that will receive all the interpolted rates
i.rates <- matrix(nrow = 35, ncol = 71)

# Actually interpolting the LFPR for all years
for (i in 1:71){
  i.rates[, i] <- approx(x = or.years, y = new_rates[, i], xout = 1979:2013)$y
}

# Note to self, i.rates has Years as rows and Ages as columns!

# No changes from this point forward, pls, pls check if everything works

f.years <- seq(2014, 2025)
ages <- seq(10, 80)

data <- i.rates[-1, ]

# Fitting the Lee-Carter model
model <- leecarter(data)

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

write.csv(f.lfpr, "forecast_rates.csv")

# Preparing Plot
library(ggplot2)

data <- t(data)
data <- as.data.frame(data)

# Write a csv file with the interpolated rates
write.csv(data, "interpolated_rates.csv")

colnames(data) <- seq(1980, 2013)
colnames(f.lfpr) <- f.years

row.names(data) <- ages
row.names(f.lfpr) <- ages

m.data <- melt(data, value.name = "LFPR", variable.name = "Year")
mf.lfpr <- melt(f.lfpr, value.name = "LFPR", variable.name = "Year")

plot.data <- rbind(m.data, mf.lfpr)
plot.data$age <- rep(seq(10, 80), each = 1, times = 46)

plot.data.select <- subset(plot.data, Year %in% c(1980, 1991, 2000, 2010, 2015, 2020, 2025))

qplot(data = plot.data.select, x = age, y = LFPR, colour = factor(Year), geom = "line", 
  main = "LFPR Brazil - Selected Years", xlab = "Ages") + facet_wrap(~Year) + theme(legend.position = "none")

qplot(data = plot.data.select, x = age, y = LFPR, colour = factor(Year), geom = "line", 
  main = "LFPR Brazil - Selected Years", xlab = "Ages") + theme(legend.title = 
                                                                  element_text(size = 12, face = "bold")) +
  scale_color_discrete(name = "Year")

qplot(data = plot.data, x = age, y = LFPR, colour = factor(Year), geom = "line", 
  main = "LFPR Brazil", xlab = "Ages") + guides(col = guide_legend(ncol = 4)) + 
  theme(legend.title = element_text(size = 12, face = "bold")) +
  scale_color_discrete(name = "Year")

parameters$age <- seq(10, 80)
qplot(data = parameters, x = age, y = model.ax, geom = "line", main = "LFPR Model ax", ylab = "ax", xlab = "Ages")

qplot(data = parameters, x = age, y = model.bx, geom = "line", main = "LFPR Model bx", ylab = "bx", xlab = "Ages")

plot.kt <- kt
plot.kt <- as.data.frame(plot.kt)
plot.kt$years <- seq(1980, 2013)
qplot(data = plot.kt, x = years, y = kt, main = "LFPR Model kt", ylab = "kt", xlab = "Ages")

plot.f.kt <- c(kt, kt.forecast, kt.lo.forecast, kt.hi.forecast)
plot.f.kt <- as.data.frame(plot.f.kt)
plot.f.kt$Type <- c(rep("Forecast", times = sum(length(kt), length(kt.forecast))),
                    rep("Low", times = length(kt.lo.forecast)),
                    rep("High", times = length(kt.hi.forecast)))
plot.f.kt$Year <- c(rep(seq(1980, 2025), times = 1),
                    rep(seq(2014, 2025), times = 1),
                    rep(seq(2014, 2025), times = 1))
colnames(plot.f.kt) <- c("kt", "Type", "Year")

qplot(data = plot.f.kt, x = Year, y = kt, geom = "line", colour = Type, main = "LFPR Model kt with Forecasts 
      and Confidence Interval", xlab = "Years") +
  scale_color_manual(values=c("#000000", "#006699", "#006699")) + 
  theme(legend.position = "none")
