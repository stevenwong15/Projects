#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic_temp.RData')

#=================================================================================
# analysis

dtrain <-
  d_all %>% 
  filter(dataset == 'train')

# baseline survival rate
dtrain %>%
group_by(Survived) %>%
summarise(n = n()) %>%
mutate(n_per = n/sum(n))
#   Survived     n     n_per
#      (dbl) (int)     (dbl)
# 1        0   549 0.6161616
# 2        1   342 0.3838384

# gender-based survival rate
dtrain %>%
group_by(Survived, Sex) %>%
summarise(n = n()) %>%
group_by(Sex) %>%
mutate(n_per = n/sum(n)) %>%
ungroup() %>%
filter(Survived == 1) %>%
  ggplot(aes(x = Sex, y = n_per)) +
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, vjust = -.1, label = str_c(round(n_per*100, 1), '%'))) +
  labs(y = 'Survival Rate')

# (gender, family)-based survival rate
dtrain %>%
group_by(Survived, Sex, Family) %>%
summarise(n = n()) %>%
group_by(Sex, Family) %>%
mutate(n_per = n/sum(n)) %>%
ungroup() %>%
filter(Survived == 1) %>%
  ggplot(aes(x = Sex, y = n_per)) +
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, vjust = -.1, label = str_c(round(n_per*100, 1), '%'))) +
  facet_grid(Family ~., labeller = label_both) + 
  labs(y = 'Survival Rate')

# (gender, family, class)-based survival rate
dtrain %>%
group_by(Survived, Sex, Family, Pclass) %>%
summarise(n = n()) %>%
group_by(Sex, Family, Pclass) %>%
mutate(n_per = n/sum(n)) %>%
ungroup() %>%
filter(Survived == 1) %>%
  ggplot(aes(x = Sex, y = n_per)) +
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, vjust = -.1, label = str_c(round(n_per*100, 1), '%'))) +
  facet_grid(Family ~ Pclass, labeller = label_both) + 
  labs(y = 'Survival Rate')
