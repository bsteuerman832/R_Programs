library(dplyr)
library(ggplot2)
set.seed(595)

dice <- data.frame(n = 
c(2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,9,9,9,9,10,10,10,11,11,12))

rolls_100 <- dice %>% sample_n(100, replace = TRUE)
barplot(table(rolls_100), main = "Rolling a dice 100 times", 
        ylab = "Frequency",
        xlab = "Dice roll")
        

# Uniform distribution notes:
# Min and max wait times for back-up that happens every 30 min
min <- 0
max <- 30

# Calculate probability of waiting less than 5 mins
prob_less_than_5 <- punif(5, min, max)
prob_less_than_5

# Calculate probability of waiting 10-20 mins
prob_between_10_and_20 <- punif(20, min, max) - punif(10, min, max)
prob_between_10_and_20

rbinom(10, 1, 0.5) # Flip a coin ten times
dbinom(9,10,0.5) # Flip a coin ten times, chance 9 are heads
pbinom(4,10,0.5) # Flip a coin ten times, chance at most 4 are heads
pbinom(4,10, 0.5, lower.tail = FALSE) # Flip a coin ten times, chance more than 4 are heads




# Normal distribution stuff:
# Let's take a dist w/ a mean of 5000 and a SD of 2000
# Probability of < 7500
pnorm(7500, mean = 5000, sd =2000)
# Probability of between 3000 and 7000
pnorm(7000, mean = 5000, sd =2000) - pnorm(3000, mean = 5000, sd =2000)



#Poisson distribution:
#Events are random but hover  around a certain lambda value:
dpois(5, lambda = 8) # Avg = 8, probability the value comes out to 5
ppois(5, lambda = 8) # Avg = 8, probability the value is <= 5
ppois(10, lambda = 8, lower.tail = FALSE) # Avg = 8, probability the value is > 10)