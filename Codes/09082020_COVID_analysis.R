#######################
## Analysis of       ##
## Registered death  ##
##    and            ##
##  Confirmed cases  ##
##                   ##
##                   ##
##  Analysis of      ##
#       the data     ##
##                   ##
#######################

# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
require(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)

# Call the data from github
my_url <- "https://raw.githubusercontent.com/tamasstahl/DA2_Assignment_COVID/master/Data/clean/covid_pop_09_08_2020_clean_final.csv"
df <- read_csv( my_url )

####
# Checking on all histograms, all three are skewed with a right tail
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()

df <- df %>% filter(death != 0 )

death_sum_stat <- df %>% summarise(
  variable = 'Number of registered deaths',
  mean = mean(death),
  median = median(death),
  std = sd(death),
  min = min(death),
  max = max(death),
  skew = skewness(death),
  NumberOfObservations = sum(!is.na(death)))

confirmed_sum_stat <- df %>% summarise(
  variable = 'Number of registered deaths',
  mean = mean(confirmed),
  median = median(confirmed),
  std = sd(confirmed),
  min = min(confirmed),
  max = max(confirmed),
  skew = skewness(confirmed),
  NumberOfObservations = sum(!is.na(confirmed))
)

sum_combined <- death_sum_stat %>% add_row(confirmed_sum_stat)
table_stat <- xtable(sum_combined, caption = "Summary statistics")

print(table_stat)

# Checking the data, found out that there are several observations with 0.
summary( df )

# Checking the 0 death observations.
df %>% filter (death == 0)

# Excluding the 0 death observations as when we would like to analyze the log the 0 values will be incomprehensible, as it would result in -Inf. 
# It could be argued that excluding these observations would change the data, but these observations were small compared to others.  
df <- df %>% filter(death != 0 )

######
# Check basic scatter-plots!
# death = alpha + beta * confirmed
#
# 1) confirmed-death: level-level model without scaling
## level-level
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (2020.08.09)",y = "Number of registered death") 

# 2) You can change the scale for confirmed cases for checking log-transformation
## log-level
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (2020.08.09, ln scale)",y = "Number of registered death") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# 3) You can change the scale for regitered for checking log-transformation
## level-log
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (2020.08.09, ln scale)",y = "Number of registered death") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# 4) You can change the scale for confirmed cases and registered deaths for checking log-transformation
## log-log
ggplot( df , aes(x = confirmed, y = death ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (2020.08.09., ln scale)",y = "Number of registered death (2020.08.09., ln scale)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans(), breaks = c(1,2,5, 10, 20, 50, 100) )

##
# After looking at the plots we could decide that the log-log transformation will be the most accurate, as it is the most linear

####
# Conclusions:
#   1) taking log of gdptot is needed, but still non-linear pattern in data/need to use 'approximation' interpretation
  #     - feasible to check and we do it due to learn how to do it, 
  #           but in practice I would skip this -> over-complicates analysis
#   2) using only gdppc is possible, but need to model the non-linearity in data
#       - Substantive: Level changes is harder to interpret and our aim is not to get $ based comparison
#       - Statistical: log transformation is way better approximation make simplification!
#   3) taking log of gdppc is making the association close to linear!
#   4) taking log for life-expectancy does not matter -> use levels!
#       - Substantive: it does not give better interpretation
#       - Statistical: you can compare models with the same y, no better fit
#       - Remember: simplest the better!
  
# Take log of death and log of confirmed cases
df <- df %>% mutate( ln_death = log( death ),
                     ln_confirmed = log( confirmed ) )
######
# Make some models:
#  ln_death and ln_confirmed:
#    reg1: ln_death = alpha + beta * ln_confirmed
#    reg2: ln_death = alpha + beta_1 * ln_confirmed + beta_2 * ln_confirmed^2
#    reg3: ln_death = alpha + beta_1 * ln_confirmed * 1(confirmed < 50) + beta_2 * ln_confirmed * 1(confirmed >= 50)
#    reg4: ln_death = alpha + beta * ln_confirmed, weights: population

###
# 1) Add powers of the variable(s) to the dataframe:
df <- df %>% mutate( ln_confirmed_sq = ln_confirmed^2,
                     ln_confirmed_cb = ln_confirmed^3)

# Regressions
# Linear model:
reg1 <- lm_robust( ln_death ~ ln_confirmed , data = df , se_type = "HC2" )
summary( reg1 )
plot_reg1 <- ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# Quadratic
reg2 <- lm_robust( ln_death ~ ln_confirmed + ln_confirmed_sq , data = df )
summary( reg2 )
plot_reg2 <- ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

# PLS:
# 1st define the cutoff for confirmed cases
cutoff <- c(100,1000)
# 2nd we use a log transformation -> cutoff needs to be transformed as well
cutoff_ln<- log( cutoff )
# Use simple regression with the lspline function
reg3 <- lm_robust(ln_death ~ lspline( ln_confirmed , cutoff_ln ), data = df )
summary( reg3 )
plot_reg3 <- ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' )

# Weighted-OLS: use reg4 setup and weight with population
reg4 <- lm_robust(ln_death ~ ln_confirmed, data = df , weights = population)
summary( reg4 )
plot_reg4 <- ggplot(data = df, aes(x = ln_confirmed, y = ln_death)) +
  geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.8,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red')+
  scale_size(range = c(0, 4)) +
  coord_cartesian(ylim = c(0, 12)) +
  labs(x = "ln(confirmed cases) ",y = "ln(registered death)")

#####
# Creating model summary with texreg
data_out <- "C:/Users/ADMIN/Desktop/CEU/R_codes/Coding 1_Assignment 2/Assigment 2/"
htmlreg( list(reg1 , reg2 , reg3 , reg4),
         type = 'html',
         custom.model.names = c("Confirmed cases - linear","Confirmed cases - quadratic", 
                                "Confirmed cases - PLS","Confirmed cases - weighted linear"),
         caption = "Modelling registered death and registered cases of COVID-19 on 2020.08.09.",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)

######
# Based on model comparison our chosen model is reg4 - ln_death ~ ln_confirmed
#   Substantive: - log-log interpretation works properly as we are interested in the percentage change of registered death and confirmed cases
#                - magnitude of coefficients are meaningful
#   Statistical: - simple model, easy to interpret
#                - Comparatively high R2 and captures variation well


######
# Residual analysis

# Get the predicted y values from the model
df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
df$reg4_res <- df$ln_death - df$reg4_y_pred 

# Find countries with largest negative errors
df %>% top_n( -5 , reg4_res ) %>% 
      select( country , death , reg4_y_pred , reg4_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg4_res ) %>% 
       select( country , death , reg4_y_pred , reg4_res )


#################################
## Testing hypothesis

# 1) Coefficient is equal to 0:
# Implemented by default...
summary( reg4 )

# 2) Coefficient is equal to your favorite value
library(car)
# Let test: H0: ln_confirmed = 0, HA: ln_confirmed neq 5
linearHypothesis( reg4 , "ln_confirmed = 0")
