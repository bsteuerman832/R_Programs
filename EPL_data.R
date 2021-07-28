# Load packages
library(rio)

rio_xlsx <- read_excel("R/EPL2021.xlsx")

# Plots a club's position in the table versus both goals scored and conceded, connected by a line
plot(rio_xlsx$GF, rio_xlsx$Pos, main = "EPL 20-21 team position vs goals scored",
     xlab = "Goals Scored",
     ylab = "Team position",
     pch = 19,
     col = "blue")

summary(lm(rio_xlsx$Pos ~ rio_xlsx$GF)) #R-squared: 0.891,
abline(lm(rio_xlsx$Pos ~ rio_xlsx$GF), col = "blue") # Slope = -0.3338

plot(rio_xlsx$GA, rio_xlsx$Pos,
     main = "EPL 20-21 team position vs goals conceded",
     xlab = "Goals conceded",
     ylab = "Team position",
       col = "red", 
       pch = 19)

summary(lm(rio_xlsx$Pos ~ rio_xlsx$GA)) #R-squared: 0.579
abline(lm(rio_xlsx$Pos ~ rio_xlsx$GA), col = "red") # Slope = 0.4

summary(rio_xlsx)

# You can also get the  correlation like this, square to get R^2:
cor(rio_xlsx$GF, rio_xlsx$Pos) # Cor = -0.9439436
cor(rio_xlsx$GA, rio_xlsx$Pos) # Cor = 0.7606566


