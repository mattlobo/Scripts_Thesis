# for this to work, first run Model Vaidation.R, then LeeCarterLFPR.R Ik it is not great, but it does the job

data <- subset(plot.data, Year %in% c(2013))
data.plot.validation <- rbind(data, plot.data.validation)
data.plot.validation$Type <- rep(c("Observed", "Forecast"), each = 71, times = 1)
data.plot.validation

qplot(data = data.plot.validation, x = age, y = LFPR, colour = factor(Type), geom = "line"
      , group = Type, main = "LFPR Brazil - Model Validation", xlab = "Age") + facet_wrap(~Year)
