# Load packages
library(rio)
library(readxl)

EPL2021 <- read_excel("R/EPL2021.xlsx")

# Plots a club's position in the table versus both goals scored and conceded, connected by a line
plot(EPL2021$GF, EPL2021$Pos, main = "EPL 20-21 team position vs goals scored",
     xlab = "Goals Scored",
     ylab = "Team position",
     pch = 19,
     col = c("lightblue", "red", "darkred", "blue", "dodgerblue", "brown4", 
             "black", "red", "gold", "blue", "lightslateblue", "gray48", 
             "yellow1", "blue", "orangered", "deepskyblue", "lightblue",
             "black", "navy", "red"))

summary(lm(EPL2021$Pos ~ EPL2021$GF)) #R-squared: 0.891,
abline(lm(EPL2021$Pos ~ EPL2021$GF), col = "blue") # Slope = -0.3338

plot(EPL2021$GA, EPL2021$Pos,
     main = "EPL 20-21 team position vs goals conceded",
     xlab = "Goals conceded",
     ylab = "Team position",
       col = c("lightblue", "red", "darkred", "blue", "dodgerblue", "brown4", 
               "black", "red", "gold", "blue", "lightslateblue", "gray48", 
               "yellow1", "blue", "orangered", "deepskyblue", "lightblue",
               "black", "navy", "red"), 
       pch = 19)

summary(lm(EPL2021$Pos ~ EPL2021$GA)) #R-squared: 0.579
abline(lm(EPL2021$Pos ~ EPL2021$GA), col = "red") # Slope = 0.4

# You can also get the  correlation like this, square to get R^2:
cor(EPL2021$GF, EPL2021$Pos) # Cor = -0.9439436
cor(EPL2021$GA, EPL2021$Pos) # Cor = 0.7606566


