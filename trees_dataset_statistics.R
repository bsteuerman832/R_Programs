library(dplyr)
library(ggplot2)

trees %>% ggplot(aes(x=Volume)) + geom_histogram()
trees %>%
summarize(mean_height = mean(trees$Height), mean_Vol = mean(trees$Volume))


#Show boxplot and calculate IQR
quantile(trees$Volume, probs = seq(0, 1, 0.1))
ggplot(trees, aes(y=Volume)) + geom_boxplot()
IQR <- quantile(trees$Volume, 0.75) - quantile(trees$Volume, 0.25)
print(IQR) # Inner Quartile Range aka The height of the boxplot



# Find variance and Standard Deviation:
trees %>% summarize(variance_Volume = var(Volume), SD_Volume = sd(Volume))




q1 <- quantile(trees$Volume, 0.25)
q3 <- quantile(trees$Volume, 0.75)
iqr <- q3 - q1
# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr
# Should show one outlier:
trees %>% filter(Volume > upper | Volume < lower)

