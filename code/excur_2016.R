# 4-15-17 
# Katie Palof
# attempt to use FSA package to do mark-recapture red crab analysis

library(FSA)
?FSA

library(fishmethods)

#Excursion 2016
#FSA

ExcP <- mrClosed(M=445, n= 212, m=53, method = "Petersen")
ExcP
summary(ExcP)

ExcC <- mrClosed(M=445, n= 212, m=53, method = "Chapman")
summary(ExcC)
#fishmethods
M <- 445
C <- 212
R <- 53
alpha = 0.05

mrN.single(M, C, R, alpha = 0.05)
# bias-corrected petersen and its variance, Seber 2002 p.60
#Bailey binomial estimator and its variance, seber 2002, p.61
