#!/usr/bin/env r -f

#install.packages("lubridate")
#install.packages("lattice")

library(lubridate)
library(lattice)

HFmax = 210

data <- read.table ("mmc.txt", header=T)
data$Date <- as.Date(data$Date)
data$Distance <- as.double(gsub(',', '', data$Distance))
data$Duration <- as.duration(hms(data$Duration))
data$Heartbeat <- as.double(data$Heartbeat)

dates <- data$Date
pace <- as.double(data$Duration/60)/(data$Distance/1000)

pdf("mmc.pdf", width = 11, height = 7)

par(mar=c(5, 4, 4, 4) + 0.1)

# Let me introduce to you the RUNNING FITNESS INDEX:
# somebody running a 5K in ONE HOUR with 100% HR ==> LFI: 0
# somebody running a 5K in TEN MINUTES with 100% HR ==> LFI: 100
rfi <- function(pace, hr, hrmax) {
    120 - 10 * (hr/hrmax)^1.25 * pace
}

rfi_data <- unlist(lapply(1:length(dates), function(i) { rfi(pace[i], data$Heartbeat[i], HFmax) }))

# Plot RFI data
panel.smoother <- function(x, y) {
  panel.xyplot(x, y, type=c("o"))	# show points 
  panel.loess(x, y, col="red")		# show smoothed line 
}
xyplot(rfi_data ~ dates, panel=panel.smoother, grid=TRUE, axes=F, xlab="", ylab="")

xyplot(data$Distance/1000 ~ dates, type=c("smooth", "o"), axes=F, xlab="", ylab="", col="green")
xyplot(data$Heartbeat ~ dates, type=c("smooth", "o"), axes=F, xlab="Date", ylab="bpm", col="red")
