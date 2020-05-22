setwd("C:\\Users\\Devam N Shah\\Desktop\\Rutgers\\Sem-2\\Applied Regression Analysis\\Project\\Final Code,Deck & Data")

library(deSolve)
library(dplyr)
library(tm)
library(ggplot2)
library(lubridate)

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta * I * S/N
    dI <- beta * I * S/N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}



#US Population - 328.2 million = 328200000

sir_start_date <- "2020-04-01"
data_covid = readxl::read_xlsx("Data.xlsx",1)
data_covid

Infected <- data_covid %>% filter(ymd(Date)>="2020-04-01" &ymd(Date)<="2020-05-02") %>% pull(Confirmed)

Day <- 1:(length(Infected))
Day

old <- par(mfrow = c(1, 2))
plot(Day, Infected, type ="b")
plot(Day, Infected, log = "y")
abline(lm(log10(Infected) ~ Day))
title("Confirmed Cases 2019-nCoV US", outer = TRUE, line = -2)

N <- 328200000
init <- c(S = N - Infected[1], I = Infected[1], R = 0)

# define a function to calculate the residual sum of squares
# (RSS), passing in parameters beta and gamma that are to be
# optimised for the best fit to the incidence data
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((Infected - fit)^2)
}

# now find the values of beta and gamma that give the
# smallest RSS, which represents the best fit to the data.
# Start with values of 0.5 for each, and constrain them to
# the interval 0 to 1.0
Opt <- optim(c(0.4,0.4), RSS, method = "L-BFGS-B", lower = c(0,0), upper = c(1, 1))

# check for convergence
Opt$message

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par

# time in days for predictions
t <- 1:as.integer(ymd("2020-05-03") - ymd(sir_start_date))

# get the fitted values from our SIR model
fitted_cumulative_incidence <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))


# add a Date column and join the observed incidence data
fitted_cumulative_incidence <- fitted_cumulative_incidence %>% 
  mutate(Date = ymd(sir_start_date) + days(t - 1))


# plot the data
fitted_cumulative_incidence %>% filter(Date <= ymd("2020-05-02")) %>% 
  ggplot(aes(x = Date)) + geom_line(aes(y = I), colour = "red") + 
  geom_point(aes(y = Infected), colour = "orange") + 
  labs(y = "Cumulative incidence", title = "COVID-19 fitted vs observed cumulative incidence", 
       subtitle = "(red=fitted incidence from SIR model, orange=observed incidence)")

R0 = Opt_par[1]/Opt_par[2]
R0