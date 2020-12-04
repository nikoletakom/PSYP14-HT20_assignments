##=====================================================================================================================================================================================================##
#   Assignment 3
##=====================================================================================================================================================================================================##

library(psych) 
library(tidyverse) 
library(lme4) 
library(lmerTest) 
library(MuMIn) 

##=====================================================================================================================================================================================================##
#   Custom functions from the seminars
##=====================================================================================================================================================================================================##

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

##=====================================================================================================================================================================================================##
#   New data set and checking for errors
##=====================================================================================================================================================================================================##

data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3")
View(data_sample_3)


data_sample_3 <- data_sample_3[-c(77, 182),] 
data_sample_3 <- data_sample_3 %>%       
  mutate(sex = factor(sex),
         hospital = factor(hospital,levels=c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9", "hospital_10")))

describe(data_sample_3)
summary(data_sample_3)

data_sample_3 %>%
  ggplot() +
  aes(y = pain, x = age) +
  geom_point(aes(color = hospital), size = 4) +
  geom_smooth(method = "lm", se = F)

plot_1 = data_sample_3 %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

plot_1+
  xlim(-1, 10)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

##=====================================================================================================================================================================================================##
#   Building a random intercept model
##=====================================================================================================================================================================================================##

mod_rand_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_sample_3)
mod_theory = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

# to compare the models' coefficients and confidence intervals
summary(mod_rand_int)
summary(mod_theory)
confint(mod_rand_int)  
confint(mod_final)

r.squaredGLMM(mod_rand_int) # marginal and conditional R squared values of the intercept model

##=====================================================================================================================================================================================================##
#   Checking data set 4
##=====================================================================================================================================================================================================##

data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4")
View(data_sample_4)

data_sample_4 %>% summary()

data_sample_4 <- data_sample_4[-c(5,80, 87),] 
data_sample_4 <- data_sample_4 %>%       
  mutate(sex = factor(sex),
         hospital = factor(hospital))

##=====================================================================================================================================================================================================##
#   New intercept and slope model on data set 4
##=====================================================================================================================================================================================================##

mod_rand_int_4 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_sample_4)
RSS_4 = sum((data_sample_4$pain - predict(mod_rand_int_4))^2)
RSS_4

mod_mean <- lm(pain ~ 1, data = data_sample_4)
TSS_4 = sum((data_sample_4$pain - predict(mod_mean))^2)

R2 = 1-(RSS_4/TSS_4)
R2 

##=====================================================================================================================================================================================================##
#   Backward regression and building the final model
##=====================================================================================================================================================================================================##

mod_rand_back = step(mod_rand_int, direction = "backward") # to find the most inlfuential predictor
mod_cortisol = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_sample_3)
mod_cortisol %>% summary()

data_sample_3 = data_sample_3 %>%
  mutate (pred_cort = predict(mod_cortisol))

plot_hosp <- data_sample_3 %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y=pred_cort, x=cortisol_serum))+
  facet_wrap( ~ hospital, ncol = 3)


