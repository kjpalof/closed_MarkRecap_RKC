#4-28-17, updated 3-19-18
# Summary of all mark recapture experiments
# chapman estimators with variance

# Load packages -----------------
library(FSA)
library(fishmethods)
library(tidyverse)
library(broom)
library(Matrix)
library(data.table)
library(extrafont)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# Load Data -------------
# input data for mark recapture experiments obtained verbally or through e-mail.  Not in database
# CSA estimates of legal crab in lbs for each year are point estimates and can be tracked in annual stock health documents
# legal weight is from the survey data and can be recalculated using raw survey data, but is tracked in Excel files for each area with the CSA estimate
# Seymour is read in here but uses a different estimator than the rest of the areas.

data <- read.csv("./data/mr_rinput.csv", header = TRUE)  # updated input file to include 2017 data
data %>% arrange(year) -> data
# M = number of animals marked on first visit
# n = number of animals captured on the second visit
# m = number of recaptured animals (marked recaped on second visit)

ExcC <- mrClosed(M=445, n= 212, m=53, method = "Chapman")
confint(ExcC)
summary(ExcC)

GambC <- mrClosed(M=888, n= 554, m=46, method = "Chapman")
confint(GambC)
summary(GambC)

data %>% 
  mutate(Chapman = (((M+1)*(n+1))/ (m+1))-1, 
         SE = (((M+1)*(n+1)*(M-m)*(n-m))/((m+1)*(m+1)*(m+2)))^0.5, 
         upper = Chapman + (SE*1.96), lower = Chapman -(SE*1.96)) ->data_sum
# summarizes Chapman estimates in number of crabs
data_sum %>% 
  mutate(Chap_lb = Chapman*legal_wt, lower_lb = lower*legal_wt, upper_lb = upper*legal_wt, 
         adj = Chap_lb/ CSA_legalcrab) -> data_sum2
# summarizes Chapman estimates in pounds of crab using average individual weight (input data) also calculates adjustment

data_sum2[c(1:4, 6:9, 12:13), ] -> data_sum_Chaponly # only the Chapman estimates

ggplot(data_sum, aes(area, Chapman))+geom_point()+geom_errorbar(ymin = data_sum$lower, ymax = data_sum$upper)

ggplot(data_sum2, aes(area, Chap_lb))+geom_point()+geom_errorbar(ymin = data_sum2$lower_lb, 
                                                                 ymax = data_sum2$upper_lb)+
  geom_point(data = data_sum, aes(area, CSA_legalcrab), colour = "red")

## Chapman only --------------
data_sum_Chaponly

ggplot(data_sum_Chaponly, aes(area, Chap_lb))+geom_point()+ scale_y_continuous(limits = c(0, 290000))+
  geom_errorbar(ymin = data_sum_Chaponly$lower_lb, ymax = data_sum_Chaponly$upper_lb)+
  geom_point(data = data_sum_Chaponly, aes(area, CSA_legalcrab), colour = "red")

# Chapman only tables -------------
data_sum2[c(1:9, 12), ] -> data_sum_Chaponly2
data_sum_Chaponly2 %>% 
  select(year, area, M, n, m, Chapman, lower, upper) ->table1


## seymour chapman estimate -----------
#Schabel Estimate
n <- c(1008,1107,146)  # number of captures
m <- c(0, 24,6)  # number of recaptures - not sure why this was 33 (instead of 24) changed back to 24. check with adam about where 33 came from
R <- c(1008, 1083, 0)  # of marked fish returned to the population
M <- c(0, cumsum(R)[-3])
(seymour15 <- data.frame(n = n, m = m, R = R, M = M))

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



# final results with score - save here
write.csv(short_term_results, './results/TCS/TCS_shortterm.csv')
