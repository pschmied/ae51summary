library(ggplot2)
library(plyr)
library(xtable)

dat <- read.csv("./data/spot-sample.csv", header=TRUE,
                stringsAsFactors=FALSE, skip=15)
dat <- dat[dat$Point != "Walk",]
dat$Point <- mapvalues(dat$Point,
                       from=c(1:4),
                       to=c("University Way", "40th St", "15th Ave", "Green wall"))

spotstats <- ddply(dat, .(Point), summarise,
                   min = min(Black.Carbon, na.rm=TRUE),
                   mean = mean(Black.Carbon, na.rm=TRUE),
                   median = median(Black.Carbon, na.rm=TRUE),
                   max = max(Black.Carbon, na.rm=TRUE))

print(xtable(spotstats, caption="Spot sample summary statistics by location", digits=0),
      file="./fig/spotstats.tex")


spotboxplot <- ggplot(dat, aes(x=Point, y=Black.Carbon)) +
  theme_bw() + geom_boxplot() + scale_shape(solid = FALSE) +
  xlab("Location") + ylab("Black Carbon Count")
ggsave("./fig/spotboxplot.pdf", width=10, height=5)


spotdist <- ggplot(dat, aes(x=Black.Carbon)) +
  theme_bw() + geom_histogram() + scale_shape(solid=FALSE) +
  xlab("Black carbon count") + ylab("# of readings")
ggsave("./fig/spotdist.pdf", width=10, height=5)
