# Load packages
rio_xlsx2 <- read_excel("R/EPL1920.xlsx")

# Plots a club's position in the table versus both goals scored and conceded, connected by a line
plot(rio_xlsx2$GF, rio_xlsx2$Pos, main = "EPL 19-20 team position vs goals scored",
     xlab = "Goals Scored",
     ylab = "Team position",
     pch = 19,
     col = "blue")

summary(lm(rio_xlsx2$Pos ~ rio_xlsx2$GF)) #R-squared: 0.725
abline(lm(rio_xlsx2$Pos ~ rio_xlsx2$GF), col = "blue") 

plot(rio_xlsx2$GA, rio_xlsx$Pos,
     main = "EPL 19-20 team position vs goals conceded",
     xlab = "Goals conceded",
     ylab = "Team position",
     col = "red", 
     pch = 19)

summary(lm(rio_xlsx2$Pos ~ rio_xlsx2$GA)) #R-squared: 0.788
abline(lm(rio_xlsx2$Pos ~ rio_xlsx2$GA), col = "red") 