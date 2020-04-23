
mobility <- read.csv("data/applemobilitytrends-2020-04-21.csv") # change path and file name accordingly

levels(mobility$region)
levels(mobility$transportation_type)

mobi_trends <- function(reg = "United States", trans = "driving", plot = TRUE, addsmooth = TRUE) {
  data <- subset(mobility, region == reg & transportation_type == trans)[4:ncol(mobility)]
  dates <- as.Date(sapply(names(data), function(x) substr(x, start = 2, stop = 11)), "%Y.%m.%d")
  values <- as.numeric(data)
  series <- setNames(values, dates)
  if (plot) {
    plot(dates, values, main = paste("Mobility Trends", reg, trans), xlab = "", ylab = "", type = "l", col = "blue", lwd = 3)
    if (addsmooth) {
      lines(dates, values, col = "lightblue", lwd = 3)
      lines(supsmu(dates, values), col = "blue", lwd = 2)
    }
    abline(h = 100)
    abline(h = c(0, 20, 40, 60, 80, 120, 140, 160, 180, 200), lty = 3)
    invisible(series)
  } else series
}


