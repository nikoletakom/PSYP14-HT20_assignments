##=====================================================================================================================================================================================================##
#  Assignment 2
##=====================================================================================================================================================================================================##

##=====================================================================================================================================================================================================##
#   Loading packages
##=====================================================================================================================================================================================================##

library(psych)
library(dplyr)
library(lm.beta) 
library(tidyverse) 
library(lmtest) 
library(car) 
library(sandwich) 

##=====================================================================================================================================================================================================##
#   Custom functions from the seminars
##=====================================================================================================================================================================================================##

error_plotter <- function(mod, col = "black", x_var = NULL){
  mod_vars = as.character(mod$call[2])
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))
  data$pred = predict(mod)
  if(x == "1" & is.null(x_var)){x = "response_ID"
  data$response_ID = 1:nrow(data)} else if(x == "1"){x = x_var}
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)
  abline(mod)
  for(i in 1:nrow(data)){
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))
    abline(v = data[i,x], lty = 2, col = col)
  }
}

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

##=====================================================================================================================================================================================================##
#  Loading the data set and checking the new predictors
##=====================================================================================================================================================================================================##

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
View(data_sample_1)
data_sample_1 <- data_sample_1[-c(93, 109, 150),] # excluding participants

data_sample_1 %>% 
  ggplot() +
  aes(x = weight, 
      y = pain,
      color = sex) +
  geom_point()

data_sample_1 %>% 
  ggplot() +
  aes(x = IQ) +
  geom_histogram()

data_sample_1 %>%
  ggplot() +
  aes(x = household_income,
      y = pain) +
  geom_point()

describe(data_sample_1)
summary(data_sample_1)

data_sample_1 %>%
  ggplot() +
  aes(y = pain, x = IQ) +
  geom_point(aes(color = sex), size = 3) +
  geom_smooth()

data_sample_1 %>%
  ggplot() +
  aes(y = pain, x = weight) +
  geom_point(aes(color = sex), size = 3) +
  geom_smooth()

data_sample_1 %>%
  ggplot() +
  aes(y = pain, x = household_income) +
  geom_point(aes(color = sex), size = 3) +
  geom_smooth()

##=====================================================================================================================================================================================================##
#   The suggested model without corstisol_saliva
##=====================================================================================================================================================================================================##

mod_hers = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1)
mod_hers %>% 
  summary()

##=====================================================================================================================================================================================================##
#   Checking for outliers in mod_hers
##=====================================================================================================================================================================================================##

mod_hers %>% plot(which = 5)            
mod_hers %>% plot(which = 4)            
data_sample_1 %>% slice(c(112, 102, 3)) 

##=====================================================================================================================================================================================================##
#   Model diagnostics on mod_hers
##=====================================================================================================================================================================================================##

#1
mod_hers %>% plot(which = 2)

residuals_mod_hers = enframe(residuals(mod_hers))
residuals_mod_hers %>%         
  ggplot() + aes(x = value) + 
  geom_histogram() 

describe(residuals(mod_hers)) 

#2
mod_hers %>% residualPlots() 

#3
mod_hers %>% plot(which = 3)
mod_hers %>% ncvTest() 
mod_hers %>% bptest() 

#4
mod_hers %>% vif() # PASS

##=====================================================================================================================================================================================================##
#   Running a backward regression
##=====================================================================================================================================================================================================##

mod_back_hers = step(mod_hers, direction = "backward")
mod_backward = lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_sample_1)
mod_theory = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

mod_backward %>% summary()
mod_theory%>% summary()
coef_table(mod_backward)

##=====================================================================================================================================================================================================##
#   AIC values for model comparison
##=====================================================================================================================================================================================================##

AIC(mod_hers)  
AIC(mod_backward)
AIC(mod_theory)

##=====================================================================================================================================================================================================##
#   Checking the new data set
##=====================================================================================================================================================================================================##

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")
View(data_sample_2)
summary(data_sample_2)

data_sample_2 <- data_sample_2 %>%       
  mutate(sex = factor(sex))

##=====================================================================================================================================================================================================##
#   Calculating the RSS values
##=====================================================================================================================================================================================================##

pred_theory <- predict(mod_theory, data_sample_2)
pred_backward <- predict(mod_backward, data_sample_2)

RSS_theory = sum((data_sample_2[, "pain"] - pred_theory)^2)
RSS_backward = sum((data_sample_2[, "pain"] - pred_backward)^2)
RSS_theory 
RSS_backward 
