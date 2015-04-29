library(ggplot2)
library(lubridate)
library(scales)

## Read our continuous sample data
c0 <- read.csv("./data/continuous-sample0.csv", header=TRUE, skip=16)
c1 <- read.csv("./data/continuous-sample1.csv", header=TRUE, skip=16)
cdat <- rbind(c0, c1)

## Concatenate and parse our date/time fields
cdat$datetime <- ymd_hms(paste(cdat$Date, cdat$Time))

## Easy to use day field
cdat$day <- wday(cdat$datetime, label=TRUE)

## Easy to use time field
cdat$time <- cdat$datetime
day(cdat$time) <- 1

## Weekend variable for ID purposes
cdat$weekend <- FALSE
cdat$weekend[cdat$day == "Sat" | cdat$day == "Sun"] <- TRUE

ptimeseries <- ggplot(data=cdat, aes(x=datetime, y=BC)) +
  geom_line() + xlab("Date/Time") +
  ylab("Black Carbon Level") + theme_bw()

ptimeseriesday <- ggplot(data=cdat, aes(x=time, y=BC, color=weekend)) +
  geom_line() + theme_bw() +
  facet_grid(Date ~ .) +
  xlab("Time") + ylab("Black Carbon Level") +
  scale_x_datetime(labels = date_format("%H:%M")) +
  scale_color_brewer(type="qual", palette=2)

ggsave("./fig/timeseries.pdf", ptimeseries, width=10, height=5)

ggsave("./fig/timeseriesday.pdf", ptimeseriesday, width=10, height=6)
