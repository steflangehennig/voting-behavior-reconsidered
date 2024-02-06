#######################
# Code updates for R&R
#######################

# Updates for the R&R 
library(descr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(haven)
library(MASS)
library(survey)
library(openxlsx)
anes <- read_dta("C:/Users/Carey/Dropbox/Research Projects/Shared Projects/Bartels Update/ANES Data/anes_timeseries_cdf_stata_20220916.dta")
setwd("C:/Users/Carey/Dropbox/Research Projects/Shared Projects/Bartels Update/Electoral-Studies/revisions/New Excels")

## creates strong, weak, lean partisan dummy var to replicate and extend Bartels (2000)
anes <- anes %>% 
  mutate(strong = case_when(
    VCF0301==7 ~ 1,
    (VCF0301 >1 & VCF0301<7) ~ 0,
    VCF0301==1 ~ -1
  ))


anes <- anes %>% 
  mutate(weak = case_when(
    VCF0301==6 ~ 1,
    (VCF0301 >2 & VCF0301<6) ~ 0,
    (VCF0301 ==1 | VCF0301==7) ~ 0,
    VCF0301==2 ~ -1
  ))


anes <- anes %>%
  mutate(lean = case_when(
    VCF0301==5 ~ 1,
    (VCF0301 <3 | VCF0301>5) ~ 0,
    VCF0301 ==4 ~ 0,
    VCF0301==3 ~ -1
  ))


anes <- anes %>% 
  mutate(rep_dem_pres = case_when(
    VCF0706==2 ~ 1,
    VCF0706==1 ~ 0
  ))

anes %>%                  
  count(rep_dem_pres) %>%                          
  mutate(percent = scales::percent(n / sum(n))) 

## recodes missing data to NAs for ideology
# note: ideology asked beginning in 1972
# value of 0 still should still be kept missing; value of 9 should be recoded per cumulative codebook

freq(anes$VCF0803)
anes$VCF0803[anes$VCF0803 == 0] <- NA

anes <- anes %>%
  mutate(ideo4 = case_when(
    (VCF0803 <=3 | VCF0803>=1) ~ 1,
    VCF0803 ==4 ~ 2,
    (VCF0803 <=7 | VCF0803>=5) ~ 3,
    VCF0803 ==9 ~ 4, 
    VCF0803 <=0 ~ NA_real_))

## Table 1 model - with coefficient graphing with CIs
# replicating Bartels and Extending for additional elections
years <- c(1952,	1956,	1960,	1964,	1968,	1972,	1976,	1980,	1984,	
           1988,	1992,	1996, 2000, 2004, 2008, 2012, 2016, 2020) # years to include in analysis 
s <- w <- l <- data.frame()
# initialize a data frame to store results
# create an empty data frame to store probit model results
results <- data.frame()

# initialize a data frame to store model evaluation metrics
model_metrics <- data.frame()

# iterate over each year
for (year in years) {
  # subset the data for the specific year
  year_data <- subset(anes, VCF0004 == year)
  
  # run the weighted probit regression
  model <- svyglm(rep_dem_pres ~ strong + weak + lean, 
                  design = svydesign(ids = ~1, weights = ~VCF0009z, 
                                     data = year_data), 
                  family = binomial(link = "probit"))
  
  # extract the coefficients
  coef_values <- coef(model)
  std_err <- sqrt(diag(vcov(model)))
  obs <- as.numeric(length(model$fitted.values))
  
  # create a new row for the coefficients data frame
  new_row <- data.frame(year = year,
                        coeff_intercept = coef_values[1],
                        coeff_strong = coef_values[2],
                        coeff_weak = coef_values[3],
                        coeff_lean = coef_values[4],
                        se_intercept = std_err[1],
                        se_strong = std_err[2],
                        se_weak = std_err[3],
                        se_lean = std_err[4],
                        n = obs,
                        stringsAsFactors = FALSE)
  
  # append the row to the coefficients data frame
  results <- rbind(results, new_row)
  
  # extract AIC
  aic_value <- model$aic
  
  # extract pseudo-R2 (McFadden's R-squared)
  pseudo_r2 <- 1 - (model$deviance / model$null.deviance)
  
  # store the model evaluation metrics
  metrics_row <- data.frame(year = year,
                            aic = aic_value,
                            pseudo_r2 = pseudo_r2,
                            stringsAsFactors = FALSE)
  
  model_metrics <- rbind(model_metrics, metrics_row)
  
  s <- rbind(s, mutate(ggpredict(model, terms = "strong", 
                                 condition = c(weak = 0, lean = 0)), year = year))
  w <- rbind(w, mutate(ggpredict(model, terms = "weak", 
                                 condition = c(strong = 0, lean = 0)), year = year))
  l <- rbind(l, mutate(ggpredict(model, terms = "lean", 
                                 condition = c(strong = 0, weak = 0)), year = year))
  
}

basic_model_metrics<-model_metrics # save final model metrics DF
basic_models<-results # save final model coefficients + SE DF
basic_combo <- left_join(basic_models, basic_model_metrics, by = "year")

## prepping predicted probabilities for graphing
s<-as.data.frame(s)
s<- s %>%  
  filter (group==1) 

s$pid<-"strong"
w<-as.data.frame(w)
w<- w %>%  
  filter (group==1)
w$pid<-"weak"
l<-as.data.frame(l)
l<- l %>%  
  filter (group==1)
l$pid<-"lean"

combo_pp <- rbind(s, w, l)

combo_pp <- combo_pp %>% # 
  mutate(x = case_when(
    x==-1 ~ 'Dem',
    x==0  ~ 'Ind',
    x==1  ~ 'Rep')) %>% 
  filter(x=='Dem' | x=='Rep')

combo_pp <- combo_pp %>% # create new variable that is voting for in-party candidate
  mutate(pred_2 = ifelse(x == "Rep", predicted, 1 - predicted)) %>% 
  mutate(ci_low = ifelse(x == "Rep", conf.low, pred_2-(predicted-conf.low))) %>% 
  mutate(ci_high = ifelse(x == "Rep", conf.high, pred_2+(predicted-conf.low)))

# reshape the data for plotting
results_long <- results[, c("year", "coeff_strong", "coeff_weak", "coeff_lean", "se_strong", "se_weak", "se_lean")]
results_long <- results_long %>%
  gather(key = "variable", value = "value", -year) 


results_long <- results_long %>%
  separate(variable, into = c("coef", "measure"), sep = "_") %>%
  pivot_wider(names_from = c(coef), values_from = value) 
results_long <- results_long %>%
  mutate(lower_ci = coeff - (1.96 * se),
         upper_ci = coeff + (1.96 * se))

graphing_t2 <- results_long %>%
  filter(measure != "intercept")

## for Reviewer 1 comment
# Plotting with error ribbons
ggplot(graphing_t2, aes(x = year, y = coeff, color = measure)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = measure), alpha = 0.2) +
  scale_color_manual(values = c("black", "darkgrey", "red")) +
  scale_fill_manual(values = c("black", "darkgrey", "red")) +
  labs(title = "Impact of PID on Voting",
       x = "Year",
       y = "Coefficient Value") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4))

# plotting with error bars
ggplot(graphing_t2, aes(x = year, y = coeff, color = measure)) +
  geom_line(linewidth=.7) +  geom_point(position = position_dodge(width = .75), 
                                        size=2) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci),
                linewidth=.5,    # thinner lines
                width=.7,
                position = position_dodge(width = .75)) +
  scale_color_manual(values = c("black", "lightcyan4", "red"), 
                     name = "PID Strength",
                     labels = c("Lean", "Strong", "Weak")) +
  labs(title = "",
       x = "Year",
       y = "Probit Coefficient") +
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme( legend.position = c(0, 1),  # adjust position (0,1) means top-left
    legend.justification = c(0, 1),  # adjust justification
    legend.direction = "horizontal")  +  
  guides(linetype = FALSE)  # remove linetype legend
    library(dplyr)

ggplot(graphing_t2, aes(x = year, y = coeff, color = measure)) +
  geom_line(linewidth=.7) +  geom_point(position = position_dodge(width = .75), 
                                        size=2) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci),
                linewidth=.5,    # thinner lines
                width=.7,
                position = position_dodge(width = .75)) +
  scale_color_manual(values = c("black", "lightcyan4", "red"), 
                     name = "PID Strength",
                     labels = c("Lean", "Strong", "Weak")) +
  labs(title = "",
       x = "Year",
       y = "Probit Coefficient") +
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme( legend.position = c(0, 1),  # adjust position (0,1) means top-left
         legend.justification = c(0, 1),  # adjust justification
         legend.direction = "horizontal")  +  
  facet_grid(measure~.) +
  guides(linetype = FALSE)  # remove linetype legend
library(dplyr)

## saving predicted probabilites for graphing 
pp_basic<- ggplot(combo_pp, aes(x = year, y = pred_2, color = pid, group =pid)) +
  facet_grid(x ~.)+
  geom_line(linewidth = 0.9) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  theme_minimal(base_size = 15) +
  scale_color_manual(values = c("black", "lightcyan4", "red"), 
                     name = "PID Strength",
                     labels = c("Lean", "Strong", "Weak")) +
  labs(x = "Year", y = "Predicted Probability Voting for In-Party Presidential Candidate", 
       title = "") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                linetype="solid",
                linewidth = 0.5,    # thinner lines
                width = 1.5,
                position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme(legend.position = c(0.35, 0.57),        # adjust position
        legend.justification = c(0, 1),        # adjust justification
        legend.direction = "horizontal", 
        axis.title.y = element_text(size = 10)) +
  scale_linetype_manual(values = c("solid", "solid", "solid"))   +  # different linetypes
  guides(linetype = FALSE)  # remove linetype legend
pp_basic



## expanded model with new ideology approach
# adding covariates to the basic Bartel's model 
# recodes missing data to NAs for ideology

freq(anes$VCF0101)

# recodes missing data to NAs for age & education 
anes$VCF0101[anes$VCF0101 == 0] <- NA
anes$VCF0110[anes$VCF0110 == 0] <- NA
anes$age<-anes$VCF0101  #age 
anes$education<-anes$VCF0110 #education 
freq(anes$VCF0803) # ideology
anes <- anes %>%
  mutate(ideo4 = case_when(
    VCF0803 ==1 ~ 'Liberal',
    VCF0803 ==2 ~ 'Liberal',
    VCF0803 ==3 ~ 'Liberal',
    VCF0803 ==4 ~ 'Moderate',
    VCF0803 ==5 ~ 'Conservative',
    VCF0803 ==6 ~ 'Conservative',
    VCF0803 ==7 ~ 'Conservative',
    VCF0803 ==9 ~ 'Other'))
anes <- anes %>% 
  mutate(college = if_else(education<3, 0, 1))
anes$ideo4<-as.factor(anes$ideo4)
anes$ideo4 <- relevel(anes$ideo4, ref = "Conservative")
anes$ideo4 <- relevel(anes$ideo4, ref = "Moderate")

freq(anes$ideo4)

# recodes race/ethnicity into dummy white or not 
anes <- anes %>% 
  mutate(white = case_when(
    VCF0106==1 ~ 1,
    (VCF0106==2 | VCF0106==3) ~ 0
  ))

# recodes gender into dummy female or not 
anes <- anes %>% 
  mutate(female = case_when(
    VCF0104==1 ~ 0,
    VCF0104==2 ~ 1
  ))

# creates South region  dummy var
anes <- anes %>% 
  mutate(south = case_when(
    VCF0112==1 ~ 0,
    VCF0112==2 ~ 0,
    VCF0112==4 ~ 0,
    VCF0112==3 ~ 1
  ))

## regression for-loop starts here 
# initialize a data frame to store results
results <- data.frame()

# initialize a data frame to store model evaluation metrics
model_metrics <- data.frame()

# create an empty data frame to store probit model results
results <- data.frame(year = numeric(),
                      coeff_intercept = numeric(),
                      coeff_strong = numeric(),
                      coeff_weak = numeric(),
                      coeff_lean = numeric(),
                      coeff_age = numeric(),
                      coeff_education = numeric(),
                      coeff_south = numeric(),
                      coeff_white = numeric(),
                      coeff_female = numeric(),
                      coeff_mod = numeric(),
                      coeff_lib = numeric(),
                      coeff_noan = numeric(),
                      se_intercept = numeric(),
                      se_strong = numeric(),
                      se_weak = numeric(),
                      se_lean = numeric(),
                      se_age = numeric(),
                      se_education = numeric(),
                      se_south = numeric(),
                      se_white = numeric(),
                      se_female = numeric(),
                      se_mod = numeric(),
                      se_lib = numeric(),
                      se_noan = numeric(),
                      n = numeric(),
                      stringsAsFactors = FALSE)

# create empty data frames to store predicted probabilities
s <- w <- l <- data.frame()

## new models with ideology - not asked prior to 1972 
years <- c(1972,	1976,	1980,	1984,	
           1988,	1992,	1996, 2000, 2004, 2008, 2012, 2016, 2020) # years to include in analysis 

# iterate over each year
for (year in years) {
  # subset the data for the specific year
  year_data <- subset(anes, VCF0004 == year)
  
  # run the weighted probit regression
  model <- svyglm(rep_dem_pres ~ strong + weak + lean + age + education +
                    south + white + female + ideo4,
                  design = svydesign(ids = ~1, weights = ~VCF0009z, 
                                     data = year_data), 
                  family = binomial(link = "probit"))
  
  # extract the coefficients and standard errors
  coef_values <- coef(model)
  std_err <- sqrt(diag(vcov(model)))
  obs <- as.numeric(length(model$fitted.values))
  
  # create a new row for the coefficients data frame
  new_row <- data.frame(
    year = year,
    intercept = coef_values[1],
    coeff_strong = coef_values[2],
    coeff_weak = coef_values[3],
    coeff_lean = coef_values[4],
    coeff_age = coef_values[5],
    coeff_education = coef_values[6],
    coeff_south = coef_values[7],
    coeff_white = coef_values[8],
    coeff_female = coef_values[9],
    coeff_mod = coef_values[10],
    coeff_lib = coef_values[11],
    coeff_noan = coef_values[12],
    se_intercept = std_err[1],
    se_strong = std_err[2],
    se_weak = std_err[3],
    se_lean = std_err[4],
    se_age = std_err[5],
    se_education = std_err[6],
    se_south = std_err[7],
    se_white = std_err[8],
    se_female = std_err[9],
    se_con = std_err[10],
    se_lib = std_err[11],
    se_noan = std_err[12],
    n = obs,
    stringsAsFactors = FALSE
  )
  
  # append the row to the coefficients data frame
  results <- rbind(results, new_row)
  
  # extract AIC
  aic_value <- model$aic
  
  # extract pseudo-R2 (McFadden's R-squared)
  pseudo_r2 <- 1 - (model$deviance / model$null.deviance)
  
  # store the model evaluation metrics
  metrics_row <- data.frame(year = year,
                            aic = aic_value,
                            pseudo_r2 = pseudo_r2,
                            stringsAsFactors = FALSE)
  
  model_metrics <- rbind(model_metrics, metrics_row)
  
  # save predicted probabilities for strong, weak, lean with year information
  s <- rbind(s, mutate(ggpredict(model, terms = "strong", 
                                 condition = c(weak = 0, lean = 0)), year = year))
  w <- rbind(w, mutate(ggpredict(model, terms = "weak", 
                                 condition = c(strong = 0, lean = 0)), year = year))
  l <- rbind(l, mutate(ggpredict(model, terms = "lean", 
                                 condition = c(strong = 0, weak = 0)), year = year))
}

fuller_model_metrics <- data.frame()
fuller_models<- data.frame()
fuller_combo <- data.frame()
fuller_model_metrics<-model_metrics # save final model metrics DF
fuller_models<-results # save final model coefficients + SE DF
fuller_combo <- left_join(fuller_models, fuller_model_metrics, by = "year")

write.xlsx(fuller_combo, file = "Fuller Model Coefficients.xlsx", 
           rowNames = FALSE)

write.xlsx(basic_combo, file = "Basic Model Coefficients.xlsx", 
           rowNames = FALSE)

s<-as.data.frame(s)
s<- s %>%  
  filter (group==1) 

s$pid<-"strong"
w<-as.data.frame(w)
w<- w %>%  
  filter (group==1)
w$pid<-"weak"
l<-as.data.frame(l)
l<- l %>%  
  filter (group==1)
l$pid<-"lean"

combo_pp_full <- rbind(s, w, l)

combo_pp_full <- combo_pp_full %>% # 
  mutate(x = case_when(
    x==-1 ~ 'Dem',
    x==0  ~ 'Ind',
    x==1  ~ 'Rep')) %>% 
  filter(x=='Dem' | x=='Rep')

write.xlsx(combo_pp_full, file = "Fuller Model PP.xlsx", 
           rowNames = FALSE)

combo_pp_full <- combo_pp_full %>% # create new variable that is voting for in-party candidate
  mutate(pred_2 = ifelse(x == "Rep", predicted, 1 - predicted)) %>% 
  mutate(ci_low = ifelse(x == "Rep", conf.low, pred_2-(predicted-conf.low))) %>% 
  mutate(ci_high = ifelse(x == "Rep", conf.high, pred_2+(predicted-conf.low)))

## graphing predicted probabilities from fuller model
# saving predicted probabilites for graphing 
pp_fuller <- ggplot(combo_pp_full, aes(x = year, y = pred_2, 
                                       color = pid, linetype = pid, 
                                       group = pid)) +
  facet_grid(x ~ .) +
  geom_line(linewidth = 0.9) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  theme_minimal(base_size = 15) +
  scale_color_manual(values = c("black", "lightcyan4", "red"), 
                     name = "PID Strength",
                     labels = c("Lean", "Strong", "Weak")) +
  labs(x = "Year", y = "Predicted Probability Voting for In-Party Presidential Candidate", 
       title = "") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                linetype="solid",
                linewidth = 0.5,    # thinner lines
                width = 1.5,
                position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme(legend.position = c(0.35, 0.57),        # adjust position
        legend.justification = c(0, 1),        # adjust justification
        legend.direction = "horizontal", 
        axis.title.y = element_text(size = 10)) +
  scale_linetype_manual(values = c("solid", "solid", "solid"))   +  # different linetypes
  guides(linetype = FALSE)  # remove linetype legend
pp_fuller


## coefficient plots for fuller model
# ideology coefficient graph
results_long <- results[, c("year", "coeff_con", "coeff_lib", "coeff_noan", "se_con", "se_lib", "se_noan")]
results_long <- results_long %>%
  gather(key = "variable", value = "value", -year) 

results_long$variable[results_long$variable == "coeff_con"] <- "coeff_mod"
results_long$variable[results_long$variable == "se_con"] <- "se_mod"

# reshape the data for Plotting
results_long <- results[, !(names(results) %in% "n")]
results_long <- results[, c("year", "coeff_strong", "coeff_weak", "coeff_lean", "se_strong", "se_weak", "se_lean")]
results_long <- results_long %>%
  gather(key = "variable", value = "value", -year) 

results_long <- results_long %>%
  separate(variable, into = c("coef", "measure"), sep = "_") %>%
  pivot_wider(names_from = c(coef), values_from = value) 
results_long <- results_long %>%
  mutate(lower_ci = coeff - (1.96 * se),
         upper_ci = coeff + (1.96 * se))

graphing_t1 <- results_long %>%
  filter(measure != "intercept")

# plotting with error bars
ggplot(graphing_t1, aes(x = year, y = coeff, color = measure)) +
  geom_line(linewidth=.7) +  geom_point(position = position_dodge(width = .75), 
                                        size=2) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci),
                linewidth=.5,    # thinner lines
                width=.7,
                position = position_dodge(width = .75)) +
  scale_color_manual(values = c("black", "lightcyan4", "red"), 
                     name = "PID Strength",
                     labels = c("Lean", "Strong", "Weak")) +
  labs(title = "",
       x = "Year",
       y = "Probit Coefficient") +
  theme_minimal(base_size = 15) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme( legend.position = c(0, 1),  # adjust position (0,1) means top-left
         legend.justification = c(0, 1),  # adjust justification
         legend.direction = "horizontal")  +  
  guides(linetype = none)  # remove linetype legend

# plotting ideology with error bars
graphing_t2<-graphing_t1 %>% 
  filter(measure=="mod" | measure=="lib")
ggplot(graphing_t2, aes(x = year, y = coeff, color = measure)) +
  geom_line(linewidth=.9) +  geom_point(position = position_dodge(width = .95), 
                                        size=2.5) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci),
                linewidth=.8,    # thinner lines
                width=.7,
                position = position_dodge(width = .95)) +
  scale_color_manual(values = c("darkgrey", "black", "red"), 
                     name = "Political Ideology: Relative to Conservatives",
                     labels = c("Liberal", "Moderate", "No Opinion")) +
  labs(title = "",
       x = "Year",
       y = "Probit Coefficient") +
  theme_minimal(base_size = 15) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme( legend.position = c(0, 1),  # adjust position (0,1) means top-left
         legend.justification = c(0, 0),  # adjust justification
         legend.direction = "horizontal")  +  
  guides(linetype = none)  # remove linetype legend

# college degree with error bars
ggplot(fuller_models, aes(x = year, y = coeff_education)) +
  geom_line(linewidth=.9) +  geom_point(position = position_dodge(width = .95), 
                                        size=2.5) +
  geom_errorbar(aes(ymin=coeff_education-(1.96*se_education), ymax=coeff_education+(1.96*se_education)),
                linewidth=.8,    # thinner lines
                width=.7,
                position = position_dodge(width = .95)) +
  scale_color_manual(values = c("darkgrey", "black", "red"), 
                     name = "Political Ideology: Relative to Conservatives",
                     labels = c("Liberal", "Moderate", "No Opinion")) +
  labs(title = "",
       x = "Year",
       y = "Probit Coefficient") +
  theme_minimal(base_size = 15) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme( legend.position = c(0, 1),  # adjust position (0,1) means top-left
         legend.justification = c(0, 0),  # adjust justification
         legend.direction = "horizontal")  +  
  guides(linetype = none)  # remove linetype legend


ggplot(graphing_t1, aes(x = year, y = coeff, shape = measure)) +
  geom_line(linewidth=.7) + geom_point(position = position_dodge(width = 0.75), size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                linewidth = 0.5,    # thinner lines
                width = 0.7,
                position = position_dodge(width = 0.75)) +
  scale_color_manual(values = c(4, 5, 7), 
                     name = "PID Strength",
                     labels = c("Lean", "Strong", "Weak")) +
  labs(title = "",
       x = "Year",
       y = "Probit Coefficient") +
  theme_minimal(base_size = 15) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4))+
  theme(legend.position = c(0.3, 1.1),        # adjust position
        legend.justification = c(0, 1),        # adjust justification
        legend.direction = "horizontal", 
        axis.title.y = element_text(size = 10)) +
  facet_grid(measure~. ) +  # facet horizontally by "pid"
  guides(linetype = FALSE)    # remove linetype legend
# flip the coordinates; remove linetype legend


## congressional updated models
anes <- anes %>% 
  mutate(rep_dem_house = case_when(
    VCF0707==2 ~ 1,
    VCF0707==1 ~ 0
  ))

anes %>%                  
  count(rep_dem_house) %>%                          
  mutate(percent = scales::percent(n / sum(n))) 

# replicating Bartels & extending for additional elections
anes2 <- subset(anes, VCF0004 > 1948) # removes unneeded replication year of 1948
anes2 <- subset(anes2, (rep_dem_house == 1 | rep_dem_house==0)) # removes unneeded replication year of 1948
years <- unique(anes2$VCF0004) # saves vector of remaining ANES survey years 

# reeplicating Bartels & extending for additional elections
years <- c(1952,	1956,	1960,	1964,	1968,	1972,	1976,	1980,	1984,	
           1988,	1992,	1996, 2000, 2004, 2008, 2012, 2016, 2020) # years to include in analysis 

# initialize a data frame to store results
# create an empty data frame to store probit model results
results <- data.frame()

# initialize a data frame to store model evaluation metrics
model_metrics <- data.frame()

# iterate over each year
for (year in years) {
  # subset the data for the specific year
  year_data <- subset(anes2, VCF0004 == year)
  
  # run the weighted probit regression
  model <- svyglm(rep_dem_house ~ strong + weak + lean, 
                  design = svydesign(ids = ~1, weights = ~VCF0009z, 
                                     data = year_data), 
                  family = binomial(link = "probit"))
  
  # extract the coefficients
  coef_values <- coef(model)
  std_err <- sqrt(diag(vcov(model)))
  obs <- as.numeric(length(model$fitted.values))
  
  # create a new row for the coefficients data frame
  new_row <- data.frame(year = year,
                        coeff_intercept = coef_values[1],
                        coeff_strong = coef_values[2],
                        coeff_weak = coef_values[3],
                        coeff_lean = coef_values[4],
                        se_intercept = std_err[1],
                        se_strong = std_err[2],
                        se_weak = std_err[3],
                        se_lean = std_err[4],
                        n = obs,
                        stringsAsFactors = FALSE)
  
  # append the row to the coefficients data frame
  results <- rbind(results, new_row)
  
  # extract AIC
  aic_value <- model$aic
  
  # extract pseudo-R2 (McFadden's R-squared)
  pseudo_r2 <- 1 - (model$deviance / model$null.deviance)
  
  # store the model evaluation metrics
  metrics_row <- data.frame(year = year,
                            aic = aic_value,
                            pseudo_r2 = pseudo_r2,
                            stringsAsFactors = FALSE)
  
  model_metrics <- rbind(model_metrics, metrics_row)

}

basic_model_metrics<-model_metrics # save final model metrics DF
basic_models<-results # save final model coefficients + SE DF
basic_combo <- left_join(basic_models, basic_model_metrics, by = "year")

write.xlsx(basic_combo, file = "congressional replication.xlsx", rowNames = FALSE)

## with ideology
anes3<-subset(anes, VCF0004>1971)
years <- unique(anes3$VCF0004) # saves vector of remaining ANES survey years 


## regression for-loop starts here 
# congressional models with covariates
# initialize a data frame to store results
results <- data.frame()

# Initialize a data frame to store model evaluation metrics
model_metrics <- data.frame()

# Create an empty data frame to store probit model results
results <- data.frame(year = numeric(),
                      coeff_intercept = numeric(),
                      coeff_strong = numeric(),
                      coeff_weak = numeric(),
                      coeff_lean = numeric(),
                      coeff_age = numeric(),
                      coeff_education = numeric(),
                      coeff_south = numeric(),
                      coeff_white = numeric(),
                      coeff_female = numeric(),
                      coeff_mod = numeric(),
                      coeff_lib = numeric(),
                      coeff_noan = numeric(),
                      se_intercept = numeric(),
                      se_strong = numeric(),
                      se_weak = numeric(),
                      se_lean = numeric(),
                      se_age = numeric(),
                      se_education = numeric(),
                      se_south = numeric(),
                      se_white = numeric(),
                      se_female = numeric(),
                      se_mod = numeric(),
                      se_lib = numeric(),
                      se_noan = numeric(),
                      n = numeric(),
                      stringsAsFactors = FALSE)

# Iterate over each year
for (year in years) {
  # Subset the data for the specific year
  year_data <- subset(anes3, VCF0004 == year)
  
  # Run the weighted probit regression
  model <- svyglm(rep_dem_house ~ strong + weak + lean + age + education +
                    south + white + female + ideo4,
                  design = svydesign(ids = ~1, weights = ~VCF0009z, 
                                     data = year_data), 
                  family = binomial(link = "probit"))
  
  # Extract the coefficients and standard errors
  coef_values <- coef(model)
  std_err <- sqrt(diag(vcov(model)))
  obs <- as.numeric(length(model$fitted.values))
  
  # Create a new row for the coefficients data frame
  new_row <- data.frame(
    year = year,
    intercept = coef_values[1],
    coeff_strong = coef_values[2],
    coeff_weak = coef_values[3],
    coeff_lean = coef_values[4],
    coeff_age = coef_values[5],
    coeff_education = coef_values[6],
    coeff_south = coef_values[7],
    coeff_white = coef_values[8],
    coeff_female = coef_values[9],
    coeff_mod = coef_values[10],
    coeff_lib = coef_values[11],
    coeff_noan = coef_values[12],
    se_intercept = std_err[1],
    se_strong = std_err[2],
    se_weak = std_err[3],
    se_lean = std_err[4],
    se_age = std_err[5],
    se_education = std_err[6],
    se_south = std_err[7],
    se_white = std_err[8],
    se_female = std_err[9],
    se_mod = std_err[10],
    se_lib = std_err[11],
    se_noan = std_err[12],
    n = obs,
    stringsAsFactors = FALSE
  )
  
  # Append the row to the coefficients data frame
  results <- rbind(results, new_row)
  
  # Extract AIC
  aic_value <- model$aic
  
  # Extract pseudo-R2 (McFadden's R-squared)
  pseudo_r2 <- 1 - (model$deviance / model$null.deviance)
  
  # Store the model evaluation metrics
  metrics_row <- data.frame(year = year,
                            aic = aic_value,
                            pseudo_r2 = pseudo_r2,
                            stringsAsFactors = FALSE)
  
  model_metrics <- rbind(model_metrics, metrics_row)

}

fuller_model_metrics <- data.frame()
fuller_models<- data.frame()
fuller_combo <- data.frame()
fuller_model_metrics<-model_metrics #Save Final Model Metrics DF
fuller_models<-results #Save Final Model Coefficients + SE DF
fuller_combo <- left_join(fuller_models, fuller_model_metrics, by = "year")


write.xlsx(fuller_combo, file = "New Congress Full Covariates with SE.xlsx", rowNames = FALSE)