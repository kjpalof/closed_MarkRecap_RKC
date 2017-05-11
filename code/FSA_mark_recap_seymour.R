#1-15-16
# Katie Palof
# attempt to use FSA package to do mark-recapture red crab analysis

library(FSA)
?FSA

library(fishmethods)

#Seymour values 2015
#FSA

Sey1 <- mrClosed(M=2091, n= 146, m=6, method = "Petersen")
Sey1
summary(Sey1)

mrClosed(M=2091, n= 146, m=6, method = "Chapman")

#fishmethods
M <- 2091
C <- 146
R <- 6
alpha = 0.05

mrN.single(M, C, R, alpha = 0.05)
# bias-corrected petersen and its variance, Seber 2002 p.60
#Bailey binomial estimator and its variance, seber 2002, p.61
