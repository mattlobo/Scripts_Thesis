setwd("~/Documents/Monografia/PNAD/Microdata/Before 2001")
year = 1992
options("scipen" = 8)
pnad.pes <- read.delim(file.choose(), quote = "")
data <- subset(pnad.pes, v0302 == 2 & v8005 >= 10)
# Recoding ages superior to 80 equl to 80
data$v8005[data$v8005 >= 80 & data$v8005 < 200] <- 80
library(plyr)
PEA <- count(data, c("v9001", "v9004", "v9115", "v8005"), wt_var = "v4729")
write.csv(PEA, paste0("PEA", year, ".csv"))
