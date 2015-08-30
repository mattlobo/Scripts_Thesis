# for this to work, first run Model Vaidation.R, then LeeCarterLFPR.R Ik it is not great, but it does the job

v.data <- subset(plot.data, Year %in% c(2010))
data.plot.validation <- rbind(v.data, plot.data.validation)
data.plot.validation$Type <- rep(c("Observed", "Forecast"), each = 81, times = 1)
data.plot.validation

qplot(data = data.plot.validation, x = age, y = log(nMx), colour = factor(Type), geom = "line",
      main = "LOG Death Rates Brazil - Model Validation", xlab = "Age")
