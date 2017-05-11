#k.palof 3-29-16
# Schnabel Estimation for seymour M-R - taken from Pop Dy class.  lab 4 - using Ben's markdown file on Git Hub

options(width = 95)
library(data.table)
#input data and create cfrog dataframe
n <- c(1008,1107,146)  # number of captures
m <- c(0, 33,6)  # number of recaptures
R <- c(1008, 1083, 0)  # of marked fish returned to the population
M <- c(0, cumsum(R)[-3])
(seymour15 <- data.frame(n = n, m = m, R = R, M = M))

#Calculate a Petersen estimate N^ and add it to the seymour15 data
seymour15$N.hat <- M * n/m  

#Calculate N* and add it to cfrog data
N.star <- ((M + 1) * (n + 1)/(m + 1)) - 1  # Chapman corrected Petersen estimate
seymour15$N.star <- c(NaN, N.star[-1])  # Need to remove the first value as it is an artifact of the Chapman correction
v.star <- (M + 1) * (n + 1) * (M - m) * (n - m)/(((m + 1)^2) * (m + 2))
seymour15$v.star <- c(NaN, v.star[-1])  # Need to remove the first value as it is an artifact of the Chapman correction
seymour15

#Mean Petersen Estimates

x.N.star <- mean(seymour15$N.star, na.rm = T)
x.v.star1 <- sum(seymour15$v.star, na.rm = T)/(length(seymour15$v.star) - 1)^2
x.v.star2 <- var(seymour15$N.star, na.rm = T)/(length(seymour15$N.star) - 1)
t <- qt((1 - 0.05/2), length(n) - 2)
se.1 <- sqrt(x.v.star1)
se.2 <- sqrt(x.v.star2)
ci.1 <- c(x.N.star - t * se.1, x.N.star + t * se.1)
ci.2 <- c(x.N.star - t * se.2, x.N.star + t * se.2)
#Mean Petersen Results

(mP.table = data.table(parameter = c("mean", "var1", "var2", "t", "se1", "se2", 
                                     "ci.1 low", "ci.1  up", "ci.2 low", "ci.2  up"), value = format(c(x.N.star, 
                                                                                                       x.v.star1, x.v.star2, t, se.1, se.2, ci.1[1], ci.1[2], ci.2[1], ci.2[2]), 
                                                                                                     scientific = FALSE, digits = 2)))
#Schabel Estimate

nM <- n * M
m.s <- sum(m)
lambda <- sum(nM)
N.schnabel <- lambda/(m.s + 1)
z <- 1.96
ci.sch <- c(lambda * (2 * m.s + z^2 - z * sqrt(4 * m.s + z^2))/(2 * m.s^2), 
            lambda * (2 * m.s + z^2 + z * sqrt(4 * m.s + z^2))/(2 * m.s^2))
#Schnabel Results

(mS.table = data.table(parameter = c("N''", "ci low", "ci  up"), value = format(c(N.schnabel, 
                                                                                  ci.sch[1], ci.sch[2]), scientific = FALSE, digits = 3)))
