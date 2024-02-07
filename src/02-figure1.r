#########################################################
# Plots for distribution of partisan strength, 1952-2020
#########################################################

library(tidyverse)
library(ggplot2)
library(ggeffects)
library(haven)
library(MASS)
library(survey)
library(openxlsx)
library(patchwork)


anes <- anes %>% 
  mutate(strong2 = case_when(
    VCF0301==7 ~ 1,
    (VCF0301 >1 & VCF0301<7) ~ 0,
    VCF0301==1 ~ 1
  ))


anes <- anes %>% 
  mutate(weak2 = case_when(
    VCF0301==6 ~ 1,
    (VCF0301 >2 & VCF0301<6) ~ 0,
    (VCF0301 ==1 | VCF0301==7) ~ 0,
    VCF0301==2 ~ 1
  ))


anes <- anes %>%
  mutate(lean2 = case_when(
    VCF0301==5 ~ 1,
    (VCF0301 <3 | VCF0301>5) ~ 0,
    VCF0301 ==4 ~ 0,
    VCF0301==3 ~ 1
  ))

anes <- anes %>%
  mutate(ind2 = case_when(
    VCF0301==5 ~ 0,
    (VCF0301 <3 | VCF0301>5) ~ 0,
    VCF0301 ==4 ~ 1,
    VCF0301==3 ~ 0
  ))

years <- c(1952,	1956,	1960,	1964,	1968,	1972,	1976,	1980,	1984,	
           1988,	1992,	1996, 2000, 2004, 2008, 2012, 2016, 2020) # years to include in analysis 

plot_data <- anes %>%
  filter(VCF0004 %in% years)

result <- plot_data %>%
  group_by(VCF0004) %>%
  summarise(
    mean_strong = weighted.mean(strong2, w = VCF0009z, na.rm = TRUE),
    mean_weak = weighted.mean(weak2, w = VCF0009z, na.rm = TRUE),
    mean_lean = weighted.mean(lean2, w = VCF0009z, na.rm = TRUE),
    mean_ind = weighted.mean(ind2, w = VCF0009z, na.rm = TRUE)
  )

ggplot(result, aes(x = VCF0004)) +
  # facet with mean_strong and mean_weak
  facet_grid(. ~ c("mean_strong", "mean_weak"), scales = "free_y") +
  
  # plot mean_strong and mean_weak
  geom_bar(aes(y = mean_strong), stat = "identity", position = "dodge", fill = "blue", color = "black") +
  geom_bar(aes(y = mean_weak), stat = "identity", position = "dodge", fill = "red", color = "black") +
  
  # add labels and title
  labs(x = "VCF0004", y = "Mean Value", title = "Vertical Facet Plot of Means") +
  
  # customize appearance
  theme_minimal()

long_result <- result %>%
  gather(key = "variable", value = "mean_value", -VCF0004)

long_result <- long_result %>%
  mutate(group = ifelse(variable %in% c("mean_strong", "mean_weak"), 1, 2))


plot_group_1 <- ggplot(long_result %>% filter(group == 1), 
                       aes(x = VCF0004, y = mean_value, 
                           color = variable, group = variable)) +
  geom_line(size=1.5) + geom_point(size=3) + 
  labs(x = "", y = "% of Electorate") +
  scale_color_manual(name = "Partisan Strength",
                     values = c("mean_strong" = "black", "mean_weak" = "grey60"),
                     labels = c("mean_strong" = "Strong", "mean_weak" = "Weak")) +
  ylim(0, 0.5) +
  theme_minimal() +
  theme(strip.text.x = element_blank(),
        strip.background = element_blank())

# create ggplot for group = 2
plot_group_2 <- ggplot(long_result %>% filter(group == 2), 
                       aes(x = VCF0004, y = mean_value, color = variable, group = variable)) +
  geom_line(size=1.5) + geom_point(size=3) + 
  labs(x = "Year", y = "% of Electorate") +
  scale_color_manual(name = "",
                     values = c("mean_lean" = "black", "mean_ind" = "grey60"),
                     labels = c("mean_lean" = "Lean", "mean_ind" = "Pure Ind")) +
  ylim(0, 0.5) +
  theme_minimal() +
  theme(strip.text.x = element_blank(),
        strip.background = element_blank())

# combine the two plots

combined_plot <- (plot_group_1 / plot_group_2) +
  plot_annotation(title = "Distribution of Party ID - 1952 to 2020", 
                  theme = theme(plot.title = element_text(hjust = 0.1)))

combined_plot 