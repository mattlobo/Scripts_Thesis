library(foreign)
library(questionr)
# Reading SPSS database
db <- file.choose()
data <- read.spss(db, to.data.frame = TRUE)
# Selecting males aged 10 yrs old and more
data <- subset(data, V2203 == "HOMEM" & V2805 >= 10)
# Recoding ages superior to 80 equl to 80
data$V2805[data$V2805 >= 80 & data$V2805 < 200] <- 80
# PEA <- wtd.table(y = data$V0501, x = data$V0805 , weights = data$V9991, normwt = FALSE, na.rm = TRUE, na.show = FALSE)
# PEA2 <- wtd.table(y = data$V0514, x = data$V0805 , weights = data$V9991, normwt = FALSE, na.rm = TRUE, na.show = FALSE)
# write.csv(PEA, "PEA1984.csv")
# write.csv(PEA2, "PEA21984.csv")

PIA <- wtd.table(y = data$V2301, x = data$V2805 , weights = data$V2997, normwt = FALSE, na.rm = TRUE, na.show = FALSE)
write.csv(PIA, "PIA1988.csv")
