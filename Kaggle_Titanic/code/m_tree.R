#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic.RData')

#=================================================================================
# data setup

# modify / select features so that they are appropriate for classification tree 
d_all_temp <-
  d_all %>%
  mutate(Sex = ifelse(Sex == 'male', 1, 0)) %>%
  # remove catagorical variables with too many catagories
  select(-c(Embarked, Cabin, Title, Ticket_A))

# catagorical variable
dummy <- dummyVars(~ Embarked + Cabin + Title + Ticket_A, data = d_all, fullRank = TRUE)
data_dummy <- tbl_df(data.frame(predict(dummy, d_all)))

d_all_mod <- bind_cols(d_all_temp, data_dummy)

#=================================================================================
# classification tree

dtrain <-
  d_all_mod %>% 
  filter(dataset == 'train') %>%
  select(-dataset, -PassengerId)

#---------------------------------------------------------------------------------
# cross-validation

n_cv <- 10
set.seed(94305)
cv <- sample(1:n_cv, nrow(dtrain), replace = T)

results <- data_frame()
for (i in 1:n_cv) {

  model <- tree(Survived ~., data = dtrain[cv!=i,])
  pred <- as.numeric(predict(model, dtrain[cv==i, ]) > 0.5)

  results <- bind_rows(results, data_frame(Survived = dtrain[cv==i, ]$Survived, pred = pred, cv = i))

}

results %>% 
group_by(cv) %>%
summarise(correct = sum(Survived == pred)/n()) %>%
summarise(mean(correct), sd(correct))
