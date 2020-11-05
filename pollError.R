library(readxl)
library(dplyr)
library(openintro)
library(ggplot2)
library(ggthemes)
library(tidyverse)
poll <- read_excel("poll.xlsx")
View(poll)

result <- read_excel("result.xlsx")
View(result)

result$dem_res <- result$dem_res * 100
result$rep_res <- result$rep_res * 100
result$net_dem_res <- result$net_dem_res * 100

result$State <- state2abbr(result$State)

pollError <- left_join(poll, result, by = "State")

dt <- pollError %>% select(State, dem_poll, rep_poll, dem_res, rep_res) %>% pivot_longer(!State, names_to = "type")

pollError <- pollError %>% mutate (predicted = ifelse (dem_poll-rep_poll>=5, "dem", ifelse(rep_poll-dem_poll>=5, "rep", "toss up"))) %>% 
  mutate (result = ifelse (dem_poll>rep_poll, "dem", "rep")) %>% mutate (CorrectStatePrediction = (result == predicted)) %>% mutate(error = net_dem_poll - net_dem_res)

ggplot(pollError, aes(State, error/100, fill=CorrectStatePrediction)) + geom_col() + 
  geom_text(aes(label = State), vjust = -0.5) +
  theme_economist() + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  ylab ("Biden's lead in polls vs. results (% difference)\n") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank(), axis.line = element_blank()) +
  labs(title = "Systematic polling error in 2020 US Presidential Election", 
       caption=" Election results from The Associated Press as of 7 pm PST on November 4th.\n Poll aggregates by Éric Grenier's Presidential Poll Tracker")

ggsave("pollingError.eps", width = 16, height = 9, units = "cm")
# 
# ggplot(pollError) + geom_point(aes(x=dem_poll, y=dem_res)) + geom_abline(slope = 1)
# 
# 
# dt %>% filter (type %in% c("dem_poll", "dem_res")) %>% ggplot() + geom_col(aes(State, value, fill=type), position = "dodge") + 
#   ylab ("Underestimated Republican Vote (% difference)") + theme_economist() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# dt %>% filter (type %in% c("rep_poll", "rep_res")) %>% ggplot() + geom_col(aes(State, value, fill=type), position = "dodge") + 
#   ylab ("Underestimated Republican Vote (% difference)") + theme_economist() +
#   theme(axis.text.x = element_text(angle = 90))
# 
