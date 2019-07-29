#' Mortality rates
#' https://stats.stackexchange.com/questions/289462/converting-annual-to-daily-mortality-rate
#' annual mortality rate is given as 3.082%
amr <- 0.03082
#' what is the daily mortality rate?
1 - (1 - amr) ^ (1 / 365)
#' or
1 - exp(1 / 365 * log (1 - amr))

#' annual mortality rate is given as 10%
amr <- 0.1
#' what is the daily mortality rate?
1 - (1 - amr) ^ (1 / 365)
#' or
1 - exp(1 / 365 * log (1 - amr))

#' what is the weekly mortality rate
#' where i is the instantaneous mortality rate per year
#' https://math.stackexchange.com/questions/3259714/how-to-convert-an-instantaneous-mortality-rate-to-a-weekly-mortality-rate
x = 0.50
1 - (1 - x) ^ (1 / 52)


#' marine mortality rate is 0.8
#' this is divided over the time at sea
#' fish migrate to sea on 1st April and return to freshwater on 1st November
#' Difference between Apr 1, 2019 and Nov 1, 2020:
#' or 82 weeks and 6 days ~ 83 weeks 
x = 0.8
1 - (1 - x) ^ (1 / 83)


