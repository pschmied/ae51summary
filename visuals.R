library(ggplot2)
library(lubridate)
library(scales)
library(pscl)
library(simcf)

##
## Data prep
## 

## Read our continuous sample data
c0 <- read.csv("./data/continuous-sample0.csv", header=TRUE, skip=16)
c1 <- read.csv("./data/continuous-sample1.csv", header=TRUE, skip=16)
cdat <- rbind(c0, c1)

## Read weather data
readweather <- function(x) {
    df <- read.csv(paste0("./data/", x, "-clean.csv"), header=FALSE)
    names(df) <- c("Time","RHum","Temp","Direct",
                   "Speed","Gust","Rain","Radiation","Pres")
    df$Date <- paste0("2015/04/", x)
    df
}
wdat <- do.call("rbind", sapply(21:27, readweather, simplify=FALSE))

## Concatenate and parse our date/time fields
cdat$datetime <- ymd_hms(paste(cdat$Date, cdat$Time))
wdat$datetime <- ymd_hms(paste(wdat$Date, wdat$Time))

## Purge seconds from weather data datetime
second(wdat$datetime) <- 0

## Easy to use day field
cdat$day <- wday(cdat$datetime, label=TRUE)

## Easy to use time field
cdat$time <- cdat$datetime
day(cdat$time) <- 1

## Weekend variable for ID purposes
cdat$weekend <- FALSE
cdat$weekend[cdat$day == "Sat" | cdat$day == "Sun"] <- TRUE

## Round negative carbon readings up to zero, because impossible. Only
## 20 zero values total after rounding, so not too many.
cdat$BC[cdat$BC < 0] <- 0

## Join carbon and weather data
dat <- merge(cdat, wdat, by="datetime")

## Add a sequential term for sensor attenuation (only one filter used
## in this sample)
dat$nob <- 1:nrow(dat)


## Code travel periods according to PSRC's trip-based time periods:
## http://www.psrc.org/assets/2938/Travel_Demand_White_Paper_2009_final.pdf
## It's a little course, but probably good enough for these purposes.
dat$Period <- "Night"                   #Base case
dat$Period[hour(dat$datetime) >= 6 & hour(dat$datetime) < 9] <- "AM Peak"
dat$Period[hour(dat$datetime) >= 9 & hour(dat$datetime) < 15] <- "Mid Day"
dat$Period[hour(dat$datetime) >= 15 & hour(dat$datetime) < 18] <- "PM Peak"
dat$Period[hour(dat$datetime) >= 18 & hour(dat$datetime) < 22] <- "Evening"
dat$Period <- as.factor(dat$Period)
dat$Period <- factor(dat$Period, c("Night", "Evening", "Mid Day",
                                   "PM Peak", "AM Peak"))


##
## Visualizations
## 
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


## Plot of black carbon levels appears / makes sense to be
## approximately Poisson distributed.
pcountsdist <- ggplot(data=dat, aes(x=BC)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(color="red") +
  theme_bw() + xlab("Black Carbon Level") +
  ylab("Frequency")

ggsave("./fig/countsdist.pdf", pcountsdist, width=10, height=5)

##
## Statistical model
##

## Definition, just in case we want to use alternate estimation
## method
mdef <- BC ~ Period + weekend + RHum + Temp + Speed + nob
compldat <- dat[complete.cases(
    subset(dat, select=c("BC", "Period", "weekend", "RHum", "Temp",
                    "Speed", "nob"))),]

mest <- glm.nb(mdef, data=compldat)

## Model fit
## Actual versus predicted plot - negative binomial
pavp <- ggplot(data.frame(actual=compldat$BC, predicted=getyhat(mest)),
                aes(x=actual, y=predicted)) +
         geom_abline(intercept=0, slope=1, color="red") +
         geom_point() + theme_bw()

ggsave(file="avp.pdf", path="fig", width=10, height=10, units=("in"))

