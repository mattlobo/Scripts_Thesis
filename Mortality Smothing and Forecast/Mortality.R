# Original Death and Population Files downloaded from lamortalidad.org
# Death Counts: http://www.lamortalidad.org/wp-content/uploads/2012/09/Brazil_Deaths.txt
# Population: http://www.lamortalidad.org/wp-content/uploads/2012/09/Brazil_Population.txt

# Reading the Death Counts
# Download the files to your working directory
# setwd("~/Documents/Monografia/Mortality")
# name the Death counts as deaths (above) for the script to run smoothly
deaths <- read.csv("Brazil_Deaths.txt", header = T, sep = "\t")

# The last Brazilian Census was 2010, so that's the last year I will use, yes ik I'm ignoring 2011, 2012, and 2013
c.deaths <- subset(deaths, Year <= 2010 & Year > 1979)

# The correction factor for Brazilian Death Counts were obttained from:
#######################################################################
# LIMA, Everton Emanuel Campos de  and  QUEIROZ, Bernardo Lanza. 
# Evolution of the deaths registry system in Brazil: associations with 
# changes in the mortality profile, under-registration of death counts, 
# and ill-defined causes of death. Cad. Saúde Pública [online]. 
# 2014, vol.30, n.8 [cited  2015-08-29], pp. 1721-1730 . 
#######################################################################

# Interpolating
correction.factors <- approx(c(1975, 1985, 1995, 2005), c(0.840, 0.901, 0.946, 0.9891), xout = seq(1980, 2005))$y

# Completeness factors per year
completeness <- c(rep(correction.factors, each = 18), rep(tail(correction.factors, 1), each = 18, times = 4), 
                  rep(1, each = 18))

# Correcting Death underegistration 
real.deaths <- c.deaths$Male/completeness

# Creating a data.frame with the new, more precise death counts
c.deaths$Male <- real.deaths 

# There's no need to keep the Female and Total death counts, sooooo... Bye
c.deaths <- c.deaths[, -c(3, 5)]

# Writing the death counts file, we will use the reshape2 package to generate a better-looking file
library(reshape2)

# write.csv(dcast(c.deaths, formula = Age ~ Year, value.var = "Male"), "death_counts.csv")

# Reading Population Census counts
pop <- read.csv("Brazil_Population.txt", header = T, sep = "\t")

# Resheping population data for interpolation, the reshape2 package was loaded previously 
# when writing the death counts file
d.data <- dcast(data = pop, formula = Year ~ Age, value.var = "Male")

# Creating matrix that will receive the interpolated population values
i.pop <- matrix(nrow = 31, ncol = 17)

# Interpolating population counts
for (i in 2:18){
  i.pop[ , i-1] <- approx(d.data$Year, d.data[ , i], xout = 1980:2010)$y
}

# Transposing matrix so that the Years are stored as columns
i.pop <- t(i.pop)

# Converting the matrix to a data.frame
i.pop <- as.data.frame(i.pop)

# Labeling the data.frame
colnames(i.pop) <- seq(1980, 2010)

# Creating the Age column
i.pop$Age <- seq(0, 80, 5)

# Resheping Interpolated population data
melt.pop.data <- melt(i.pop, id.vars = "Age", variable.name = "Year", value.name = "Population")
row.names(melt.pop.data) <- NULL

# Writing file

# write.csv(dcast(melt.pop.data, formula = Age ~ Year, value.var = "Population"), "Interpolated.Population.csv")

# Creating plots

library(ggplot2)

qplot(data = melt.pop.data, x = Age, y = Population, geom = "line", colour = factor(Year), 
      main = "Brazilian Population: 1980 - 2010") + guides(col = guide_legend(ncol = 2))

PIA <- c()
for (i in 1:nrow(melt.pop.data)) {
  if (melt.pop.data$Age[i] >= 10) {
    ifelse (melt.pop.data$Age[i] < 65, PIA[i] <- melt.pop.data$Population[i], PIA[i] <- NA)
  } 
}
PIA <- as.data.frame(PIA)
PIA$Year <- melt.pop.data$Year
PIA <- na.exclude(PIA)

tapply(PIA$PIA, PIA$Year, FUN = sum)


Old.Age <- c()
for (i in 1:nrow(melt.pop.data)) {
  if (melt.pop.data$Age[i] >= 65) {
    Old.Age[i] <- melt.pop.data$Population[i]
  }
}
Old.Age <- as.data.frame(Old.Age)
Old.Age$Year <- melt.pop.data$Year 
Old.Age <- na.exclude(Old.Age)

tapply(Old.Age$Old.Age, Old.Age$Year, FUN = sum)

Percentage <- tapply(Old.Age$Old.Age, Old.Age$Year, FUN = sum) / tapply(PIA$PIA, PIA$Year, FUN = sum)
Dep.Ratio <- as.data.frame(Percentage)
Dep.Ratio$Year <- seq(1980, 2010, 1)
Dep.Ratio$Type <- rep("Dependency Ratio", 31)


Contribuintes <- c(53964928, 55877835, 60197924, 64109870, 67246063, 69669481)
Beneficiários <- c(25975630, 26831267, 27999034, 28909419, 29883423, 31028250)

SS.Ratio <- Contribuintes/Beneficiários

qplot(data = Dep.Ratio, x = Year, y = Percentage)