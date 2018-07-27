library(tidyverse)

#truncate data to top and bottom 10 states per year in dollars
top_10 <- NIH_annual %>%
  group_by(yr) %>%
  top_n(., 10, fund_tot) %>%
  summarise(sum = sum(fund_tot)) %>%
  ungroup()

bot_10 <- NIH_annual %>%
  group_by(yr) %>%
  top_n(., -10, fund_tot) %>%
  summarise(sum = sum(fund_tot)) %>%
  ungroup()

top_10$yr <- as.numeric(top_10$yr)
bot_10$yr <- as.numeric(bot_10$yr)

#plot the data
ggplot() +
  geom_line(data = top_10, aes(x = yr, y = sum/10^6, group = 1), color = "blue", size = 1) +
  geom_line(data = bot_10, aes(x = yr, y = sum/10^4, group = 1), color = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Funding of bottom 10 in tens of thousands"), breaks = ) +
  labs(y = "Funding of top 10 in millions") +
  theme_bw() +
  theme(axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"))
  
ggplot() +
  geom_line(data = top_10, aes(x = yr, y = sum/10^6, group = 1), color = "blue", size = 1) +
  geom_line(data = bot_10, aes(x = yr, y = sum/10^6, group = 1), color = "red", size = 1) +
  #scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Funding of bottom 10 in tens of thousands"), breaks = ) +
  labs(y = "Funding of top 10 in millions") +
  theme_bw() +
  theme(axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"))


bot_10_names <- NIH_annual %>%
  group_by(yr) %>%
  top_n(., -10, fund_tot) 


bot_10_cont <- NIH_annual %>%
  filter(STATE != "GU", STATE != "VI", STATE != "DC", STATE != "PR") %>%
  group_by(yr) %>%
  top_n(., -10, fund_tot) %>%
  summarise(sum = sum(fund_tot)) 

top_10_cont <- NIH_annual %>%
  filter(STATE != "GU", STATE != "VI", STATE != "DC", STATE != "PR") %>%
  group_by(yr) %>%
  top_n(., 10, fund_tot) %>%
  summarise(sum = sum(fund_tot)) %>%
  ungroup()

top_10_cont$yr <- as.numeric(top_10_cont$yr)
bot_10_cont$yr <- as.numeric(bot_10_cont$yr)

ggplot() +
  geom_line(data = top_10_cont, aes(x = yr, y = sum/10^6, group = 1), color = "blue", size = 1) +
  geom_line(data = bot_10_cont, aes(x = yr, y = sum/10^6, group = 1), color = "red", size = 1) +
  #scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Funding of bottom 10 in tens of thousands"), breaks = ) +
  labs(y = "Funding of top 10 in millions") +
  theme_bw() +
  theme(axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"))

# $/award

NIH_annual <- NIH_annual %>%
  mutate(moneyrat = fund_tot/numaward_tot)

NIH_annual$yr <- as.numeric(NIH_annual$yr)


