library(readxl)
library(dplyr)
library(openintro)
library(ggplot2)
library(ggthemes)
library(tidyverse)
# poll <- read_excel("poll.xlsx")
# View(poll)


fivethirtyeight <- read_csv("presidential_poll_averages_2020.csv") %>% filter (modeldate=="11/3/2020") %>%
                   filter (candidate_name == "Joseph R. Biden Jr." | candidate_name == "Donald Trump") %>%
                   select (-cycle, -modeldate, -pct_trend_adjusted) %>% pivot_wider(id_cols = "state", names_from = "candidate_name", values_from="pct_estimate") %>% 
                   rename (State=state) %>% rename(dem_poll = `Joseph R. Biden Jr.`) %>% rename(rep_poll = `Donald Trump`) %>% mutate(net_dem_poll = dem_poll - rep_poll) %>%
                   filter(!(State %in% c("NE-1", "NE-2", "ME-1", "ME-2", "National") ))

 result <- read_excel("result.xlsx")

 result$dem_res <- result$dem_res * 100
 result$rep_res <- result$rep_res * 100
 result$net_dem_res <- result$net_dem_res * 100

poll<- fivethirtyeight
result$State <- state2abbr(result$State)
poll$State<- state2abbr(poll$State)

pollError <- left_join(poll, result, by = "State") #%>% filter(State!="AK")

dt <- pollError %>% select(State, dem_poll, rep_poll, dem_res, rep_res) %>% pivot_longer(!State, names_to = "type")

pollError <- pollError %>% mutate (predicted = ifelse (dem_poll-rep_poll>=3, "dem", ifelse(rep_poll-dem_poll>=3, "rep", "toss up"))) %>% 
  mutate (result = ifelse (dem_res-rep_res>=3, "dem", ifelse(rep_res-dem_res>=3, "rep", "toss up")))  %>% mutate (CorrectStatePrediction = (result == predicted)) %>% mutate(error = net_dem_poll - net_dem_res)

ggplot(pollError, aes(State, error/100, fill=CorrectStatePrediction)) + geom_col() + 
  geom_text(aes(label = State), vjust = -0.5,  size = 3 ) +
  theme_economist() + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
 # ylab ("Biden's lead in polls vs. results (% difference)\n") +
  #ylim(-0.06,0.25) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank(), axis.line = element_blank()) #+
  # labs(title = "Systematic polling error in 2020 US Presidential Election", 
  #      caption="Poll aggregates by FiveThirtyEight\n Election results from The Associated Press as of 7 pm PST on November 4th.\n Visualization by Shefa Analytics. More information can be found on Shefa.ca ")

ggsave("pollingError.eps")
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
