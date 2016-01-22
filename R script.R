#1-21-16
#Katie Palof, ADF&G 

#Explore closed population estimates for RKC mark recapture
#load librarys
library(FSA)
library(FSAdata)

#Seymour 2015 use for example.
# M = 2091 #marked in first occasion
# R = 6 #recaptured in second occasion
# C = 153 # total caught in second occasion.
#Peterson Method




#example
pikech1 <- capHistSum(PikeNYPartial1, cols2use =2:5)
pikech1$caphist
pikech1$sum
