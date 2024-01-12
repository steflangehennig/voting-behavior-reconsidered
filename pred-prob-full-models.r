####################################
# Code for fully specified models
####################################

# Initialize a data frame to store results
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

# Create empty data frames to store predicted probabilities
s <- w <- l <- data.frame()

### New Models with Ideology - Not Asked prior to 1972 
years <- c(1972,	1976,	1980,	1984,	
           1988,	1992,	1996, 2000, 2004, 2008, 2012, 2016, 2020) # Years to include in analysis 

# Iterate over each year
for (year in years) {
  # Subset the data for the specific year
  year_data <- subset(anes, VCF0004 == year)
  
  # Run the weighted probit regression
  model <- svyglm(rep_dem_pres ~ strong + weak + lean + age + education +
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
    coeff_con = coef_values[10],
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
  
  # Save predicted probabilities for strong, weak, lean with year information
  s <- rbind(s, mutate(ggpredict(model, terms = "strong", 
                                 condition = c(weak = 0, lean = 0)), year = year))
  w <- rbind(w, mutate(ggpredict(model, terms = "weak", 
                                 condition = c(strong = 0, lean = 0)), year = year))
  l <- rbind(l, mutate(ggpredict(model, terms = "lean", 
                                 condition = c(strong = 0, weak = 0)), year = year))
}

fuller_model_metrics<-model_metrics #Save Final Model Metrics DF
fuller_models<-results #Save Final Model Coefficients + SE DF
fuller_combo <- left_join(fuller_models, fuller_model_metrics, by = "year")

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

full_results<-results 

combo_pp <- combo_pp %>% #Create new variable that is voting for in-party candidate
  mutate(pred_2 = ifelse(x == "Rep", predicted, 1 - predicted)) %>% 
  mutate(ci_low = ifelse(x == "Rep", conf.low, pred_2-(predicted-conf.low))) %>% 
  mutate(ci_high = ifelse(x == "Rep", conf.high, pred_2+(predicted-conf.low)))

# Reshape the data for Plotting 
results_long <- results %>%
  gather(key = "variable", value = "value", -year) 

results_long <- results_long %>%
  separate(variable, into = c("coef", "measure"), sep = "_") %>%
  pivot_wider(names_from = c(coef), values_from = value) 
results_long <- results_long %>%
  mutate(lower_ci = coeff - (1.96 * se),
         upper_ci = coeff + (1.96 * se))

graphing_t1 <- results_long %>%
  filter(measure != "intercept")

####For Reviewer 1 comment
# Plotting with Error Ribbons
ggplot(graphing_t1, aes(x = year, y = coeff, color = measure)) +
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

# Plotting with Error Bars
ggplot(graphing_t1, aes(x = year, y = coeff, color = measure)) +
  geom_line() + geom_point()+
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci),
                linewidth=.3,    # Thinner lines
                width=.2)  + 
  scale_color_manual(values = c("black", "darkgrey", "red"), 
                     name = "PID Strength",
                     labels = c("Lean", "Strong", "Weak")) +
  scale_fill_manual(values = c("black", "darkgrey", "red")) +
  labs(title = "Impact of PID on Voting",
       x = "Year",
       y = "Probit Coefficient") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme( legend.position = c(0, 1),  # Adjust position (0,1) means top-left
         legend.justification = c(0, 1),  # Adjust justification
         legend.direction = "horizontal")  # Make the legend horizontal 
library(dplyr)

####Saving Predicted Probabilites for Graphing 
pp_basic<- ggplot(combo_pp, aes(x = year, y = pred_2, color = pid, group =pid)) +
  facet_grid(x ~.)+
  geom_line(linewidth=.7) + geom_point() +   
  theme_minimal(base_size = 10) +
  scale_color_manual(values = c("black", "darkgrey", "red"), 
                     name = "PID Strength",
                     labels = c("Lean", "Strong", "Weak")) +
  labs(x = "Year", y = "Predicted Probability", 
       title = "Probability Voting for In-Party Presidential Candidate") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high),
                linewidth=.3,    # Thinner lines
                width=.2) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme( legend.position = c(.5, .60),  # Adjust position (0,1) means top-left
         legend.justification = c(0, 1),  # Adjust justification
         legend.direction = "horizontal")  
pp_basic