######################################
# Plots for bivariate and full models
######################################

library(ggplot2)
library(readr)
library(ggExtra) 


full <- read_csv(here::here("data/pres-weighted.csv")) # old data
full <- read_csv(here::here("data/pres-weighted-rr.csv")) # R&R data

### with legend ###

p2 <- ggplot() + 
  geom_line(data=full, aes(x=year, y=pres_no_control, color='pres_no_control', linetype='pres_no_control'),size=.82)+
  geom_line(data=full, aes(x=year, y=pres_control, color='pres_control', linetype='pres_control'),size=.82)+
  scale_x_continuous(breaks = seq(1952,2020,4)) +
  labs(x = 'Year') +
  labs(y = 'Partisan Importance') +
  scale_colour_manual(values = c('pres_no_control' = 'darkslategray', 'pres_control' = 'black'), 
                      guide = 'none') +
  scale_linetype_manual('', labels = c('Additional Controls', 'No Controls'), 
                        values = c('pres_no_control' = 'solid', 'pres_control' = 'dashed')) +
  theme_light()

p2 +  scale_y_continuous(breaks = seq(0, 2, 0.1)) + removeGrid(y=FALSE) + expand_limits(y = 0)