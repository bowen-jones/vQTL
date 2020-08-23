# Noticed different rates of increase in plate data. Not all samples exponential.
library(ggplot2)
dat <- read.csv("data/2018_11/2018_11_1_plate.csv")
dat <- dat[-c(1:5),]
names(dat) <- c("cycle", "a1", "a2", "a3", "a4", "a5", "a6")
dat$cycle <- as.numeric(as.character(dat$cycle))
dat$a1 <- as.numeric(as.character(dat$a1))
dat$a2 <- as.numeric(as.character(dat$a2))
dat$a3 <- as.numeric(as.character(dat$a3))
dat$a4 <- as.numeric(as.character(dat$a4))
dat$a5 <- as.numeric(as.character(dat$a5))

ggplot(dat, aes(cycle, a1)) +
  geom_point() +
  geom_line()
ggplot(dat, aes(cycle, a2)) +
  geom_point() +
  geom_line()
ggplot(dat, aes(cycle, a3)) +
  geom_point() +
  geom_line()
ggplot(dat, aes(cycle, a4)) +
  geom_point() +
  geom_line()
ggplot(dat, aes(cycle, a5)) +
  geom_point() +
  geom_line()