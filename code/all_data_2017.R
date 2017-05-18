#4-28-17
# Summary of all mark recapture experiments
# chapman estimators with variance

# Load packages -----------------
library(FSA)
library(fishmethods)
library(tidyverse)
library(broom)
library(Matrix)

# Load Data -------------
data <- read.csv("./data/mr_rinput.csv", header = TRUE)  
# M = number of animals marked on first visit
# n = number of animals captured on the second visit
# m = number of recaptured animals (marked recaped on second visit)

ExcC <- mrClosed(M=445, n= 212, m=53, method = "Chapman")
confint(ExcC)
summary(ExcC)

data %>% 
  mutate(Chapman = (((M+1)*(n+1))/ (m+1))-1, 
         SE = (((M+1)*(n+1)*(M-m)*(n-m))/((m+1)*(m+1)*(m+2)))^0.5, 
         upper = Chapman + (SE*1.96), lower = Chapman -(SE*1.96)) ->data_sum

ggplot(data_sum, aes(area, Chapman))+geom_point()+geom_errorbar(ymin = data_sum$lower, ymax = data_sum$upper)




### Not sure how to do it this way  ------------
data %>% # doesn't work with dat2 data because there are no 0's for missing data
  group_by(year, area) %>%
  do(fit = mrClosed (M =data$M, n=data$n, m=data$m , method = "Chapman")) -> Chapman


  
broom:::tidy.dgCMatrix(Chapman)

Chapman %>%
  tidy(fit) -> step1

short_term %>%
  glance(fit) ->short_term_out

recruit_used <- c("Large.Females",  "Pre_Recruit", "Recruit","Post_Recruit")
short_term_out %>%
  filter(mod_recruit %in% recruit_used) %>%
  select(Location, mod_recruit, r.squared, p.value)->short_term_out2

short_term_slope %>%
  filter(mod_recruit %in% recruit_used, term == 'Year') %>%
  select(Location, mod_recruit, estimate) %>%
  right_join(short_term_out2)->short_term_results # estimate here is slope from regression
#Now need to add column for significance and score
short_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & estimate > 0, 1,
                              ifelse(p.value <0.05 & estimate <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> short_term_results #estimate is slope from regression
# final results with score - save here
write.csv(short_term_results, './results/TCS/TCS_shortterm.csv')