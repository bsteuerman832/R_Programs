# Load packages
library(rio)

info <- import("C:/Users/Brian Steuerman/Desktop/miles.xlsx")
info <- info[order(-info$miles),]

average <- sum(info[, 2]) / length(info$Date)
print(paste("Average: ", average))

colors <- c("blue", "yellow", "green", "black", "red", "purple")
pie (info$miles, label = info$Date, main = "Miles driven by date", col = colors)
freq <- as.data.frame(table(info$miles)) #Gets frequency of the # of miles driven
barplot(freq[,2], names.arg = freq[,1], col = "red", xlab = "Miles driven", ylab = "Number of unique days driven")
summary(info)
