#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic.RData')

#=================================================================================
# exploratory data analysis - plots

d_all_temp <-
  d_all %>%
  mutate(Sex = ifelse(Sex == 'male', 1, 0),
         Survived = as.factor(Survived)) %>%
  # remove catagorical variables with too many catagories
  select(-c(Embarked, Cabin, Title, Ticket_A))

# catagorical variable
dummy <- dummyVars(~ Embarked + Cabin + Title, data = d_all, fullRank = TRUE)
data_dummy <- tbl_df(data.frame(predict(dummy, d_all)))

d_all_mod <- bind_cols(d_all_temp, data_dummy)

#---------------------------------------------------------------------------------
# correlation

# Survived somewhat correlates with Pclass and Fare
# so we should not immediately reject linear methods
dtrain_cor <- 
  d_all_mod %>%
  filter(dataset == 'train') %>%
  mutate(Survived = as.integer(Survived)) %>%
  select(-c(PassengerId, dataset)) %>%
  cor()

dtrain_cor_plot <-
  tbl_df(data.frame(dtrain_cor, FEATURE_Y = rownames(dtrain_cor))) %>%
  gather(FEATURE_X, VALUE, -FEATURE_Y)

ggplot(dtrain_cor_plot, aes(x = FEATURE_X, y = FEATURE_Y)) + 
  geom_tile(aes(fill = VALUE)) + 
  scale_fill_distiller(palette = 'RdBu') + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = '', y = '')
save_as('graphics/corr.pdf', 10, 10)

#---------------------------------------------------------------------------------
# PCA

d_pca <- 
  d_all_mod %>%
  filter(dataset == 'train') %>%
  select(-c(PassengerId, dataset))

pca.out <- prcomp(d_pca %>% select(-Survived), scale = T)

pca.out_point <- tbl_df(data.frame(pca.out$x, Survived = d_pca$Survived))
pca.out_line <- tbl_df(data.frame(pca.out$rotation*10, FEATURE = rownames(pca.out$rotation)))

ggplot(pca.out_point, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.5, aes(color = Survived)) + 
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               data = pca.out_line, arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(aes(label = FEATURE, x = PC1*1.1, y = PC2*1.1), data = pca.out_line) +
  labs(x = 'PC1', y = 'PC2', title = 'Arrows * 10') +
  expand_limits(x=c(-10,10), y=c(-10,10)) +
  scale_colour_brewer(palette='Set1')
save_as('graphics/pca.pdf', 10, 10)
