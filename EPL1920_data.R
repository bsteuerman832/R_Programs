# Load packages
library(calibrate)
EPL1920 <- read_excel("R/EPL1920.xlsx")

names = c("LIV","MCI","MU","CHE","LEI","TOT","WOL","ARS","SHU","BUR","SOU","EVE","NEW","CRY","BHA",
          "WHU","AVL","BOU","WAT","NOR")

# Plots a club's position in the table versus both goals scored and conceded, connected by a line
plot(EPL1920$GF, EPL1920$Pos, main = "EPL 19-20 team position vs goals scored",
     xlab = "Goals Scored",
     ylab = "Team position",
     pch = 19,
     col = c("darkred", "lightblue", "red", "blue", "dodgerblue", "black", 
             "yellow1", "red", "red", "lightblue", "orangered", "blue", 
             "gray48", "blue", "orangered", "brown4", "lightslateblue",
             "red", "yellow", "green3"))

text(EPL1920$GF, EPL1920$Pos + 0.5, labels = names)

summary(lm(EPL1920$Pos ~ EPL1920$GF)) #R-squared: 0.725
abline(lm(EPL1920$Pos ~ EPL1920$GF), col = "blue") 

plot(EPL1920$GA, EPL1920$Pos,
     main = "EPL 19-20 team position vs goals conceded",
     xlab = "Goals conceded",
     ylab = "Team position",
     col = c("darkred", "lightblue", "red", "blue", "dodgerblue", "black", 
             "yellow1", "red", "red", "lightblue", "orangered", "blue", 
             "gray48", "blue", "orangered", "brown4", "lightslateblue",
             "red", "yellow", "green3"), 
     pch = 19)

text(EPL1920$GA, EPL1920$Pos + 0.5, labels = names)

summary(lm(EPL1920$Pos ~ EPL1920$GA)) #R-squared: 0.788
abline(lm(EPL1920$Pos ~ EPL1920$GA), col = "red") 

cor(EPL1920$GF, rio_xlsx$Pos) # Cor = -0.8513457
cor(EPL1920$GA, rio_xlsx$Pos) # Cor = 0.8879449
