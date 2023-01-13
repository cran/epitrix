## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=7,
  fig.height=5,
  fig.path="figs-overview/"
)

## ---- echo = TRUE, message=FALSE----------------------------------------------
library(magrittr)
library(epitrix)
library(distcrete)
library(ggplot2)

## ---- echo = TRUE-------------------------------------------------------------
ll <- sim_linelist(15)

x <- 0:15
y <- distcrete("gamma", 1, shape = 12, rate = 3, w = 0)$d(x)
mkexposures <- function(i) {
  i - sample(x, size = sample.int(5, size = 1), replace = FALSE, prob = y)
}
exposures <- sapply(ll$date_of_onset, mkexposures)
ll$dates_exposure <- exposures

print(ll)

## -----------------------------------------------------------------------------
incubation_period_dist <- empirical_incubation_dist(ll, date_of_onset, dates_exposure)
print(incubation_period_dist)

ggplot(incubation_period_dist, aes(incubation_period, relative_frequency)) +
  geom_col()

## -----------------------------------------------------------------------------
fit <- fit_gamma_incubation_dist(ll, date_of_onset, dates_exposure)
print(fit)

x = c(0:10)
y = fit$distribution$d(x)
ggplot(data.frame(x = x, y = y), aes(x, y)) +
  geom_col(data = incubation_period_dist, aes(incubation_period, relative_frequency)) +
  geom_point(stat="identity", col = "red", size = 3) +
  geom_line(stat="identity", col = "red")

