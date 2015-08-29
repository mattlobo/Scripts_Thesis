library(questionr)
year = 1990
options("scipen" = 8)
pnad.pes <- read.delim(file.choose(), quote = "")
data <- subset(pnad.pes, v0303 == 1 & v0805 >= 10)
# Recoding ages superior to 80 equl to 80
data$v0805[data$v0805 >= 80 & data$v0805 < 200] <- 80
PEA <- wtd.table(y = data$v0501, x = data$v0805 , weights = data$v3091, normwt = FALSE, na.rm = TRUE, na.show = FALSE)
write.csv(PEA, paste0("PEA", 1999, ".csv"))
