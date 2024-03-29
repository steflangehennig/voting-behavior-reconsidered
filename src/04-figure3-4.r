############################################
# Partisanship and Voting Behavior 
# Reconsidered in the Age of Polarization
# Accepted at Electoral Studies 10 Feb 2024
# Plot code for predicted probabilities
############################################


library(tidyverse)
library(ggplot2)
library(ggeffects)
library(haven)
library(MASS)
library(survey)
library(openxlsx)
library(patchwork)


# reshape data for plotting: data come from the 'full results' in file 01-pred-prob-full-model
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


# plotting with error ribbons
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

# plotting with error bars
ggplot(graphing_t1, aes(x = year, y = coeff, color = measure)) +
  geom_line() + geom_point()+
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci),
                linewidth=.3,    
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
  theme( legend.position = c(0, 1), 
         legend.justification = c(0, 1),  
         legend.direction = "horizontal")  
library(dplyr)

## saving predicted probabilites for graphing 
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
                linewidth=.3,    # thinner lines
                width=.2) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4),
                     minor_breaks = seq(1952, 2020, by = 4)) +
  theme( legend.position = c(.5, .60),  
         legend.justification = c(0, 1), 
         legend.direction = "horizontal")  
pp_basic

# plotting ideology with error bars
graphing_t2<-graphing_t1 %>% 
  filter(measure=="mod" | measure=="lib")
ggplot(graphing_t2, aes(x = year, y = coeff, color = measure)) +
  geom_line(linewidth=.9) +  geom_point(position = position_dodge(width = .95), 
                                        size=2.5) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci),
                linewidth=.8,  
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
  theme( legend.position = c(0, 1),  
         legend.justification = c(0, 0),  
         legend.direction = "horizontal")  +  
  guides(linetype = none)  
