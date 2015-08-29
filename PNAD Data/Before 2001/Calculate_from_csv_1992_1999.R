library(questionr)
year = 1996
options("scipen" = 8)
pnad.pes <- read.delim(file.choose(), quote = "")
data <- subset(pnad.pes, v0302 == 2 & v8005 >= 10)
# Recoding ages superior to 80 equl to 80
data$v8005[data$v8005 >= 80 & data$v8005 < 200] <- 80
PEA1 <- wtd.table(y = data$v9001, x = data$v8005, weights = data$v4729, normwt = FALSE, na.rm = TRUE, na.show = FALSE)
write.csv(PEA1, paste0("PEA", year, ".csv"))
