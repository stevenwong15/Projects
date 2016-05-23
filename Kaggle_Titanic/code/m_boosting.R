#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic.RData')

#=================================================================================
# data setup

# modify / select features so that they are appropriate for boosting
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
# boosting

dtrain <-
  d_all_mod %>% 
  filter(dataset == 'train') %>%
  select(-dataset, -PassengerId)

dtest <-
  d_all_mod %>% 
  filter(dataset == 'test') %>%
  select(-dataset)

#---------------------------------------------------------------------------------
# cross-validation

n_cv <- 10
set.seed(94305)
cv <- sample(1:n_cv, nrow(dtrain), replace = T)

results <- data_frame()
for (i in 1:n_cv) {

  train <- xgb.DMatrix(
    data = as.matrix(select(dtrain[cv != i, ], -Survived)), 
    label = as.matrix(select(dtrain[cv != i, ], Survived))
    )

  test <- xgb.DMatrix(
    data = as.matrix(select(dtrain[cv == i, ], -Survived))
    )

  # boosting
  model <- xgb.train(data = train, objective = 'binary:logistic', booster = 'gbtree',
                     nrounds = 2500, max_depth = 10)

  # # random forest
  # model <- xgb.train(data = train, objective = 'binary:logistic', 
  #                    nrounds = 1, num_parallel_tree = 5000, colsample_bytree = 0.25)

  pred <- as.numeric(predict(model, test) > 0.5)

  results <- bind_rows(
    results, 
    data_frame(Survived = dtrain[cv==i, ]$Survived, pred = pred, cv = i)
    )

}

results %>% 
group_by(cv) %>%
summarise(correct = sum(Survived == pred)/n()) %>%
summarise(mean(correct), sd(correct))

# #---------------------------------------------------------------------------------
# # result

# train <- xgb.DMatrix(
#   data = as.matrix(select(dtrain, -Survived)), 
#   label = as.matrix(select(dtrain, Survived))
#   )

# test <- xgb.DMatrix(
#   data = as.matrix(select(dtest, -Survived, -PassengerId))
#   )

# model <- xgb.train(data = train, objective = 'binary:logistic', booster = 'gbtree',
#                    nrounds = 2500, max_depth = 10)
# pred <- as.numeric(predict(model, test) > 0.5)
# output <- bind_cols(select(dtest, PassengerId), data_frame(Survived = pred))
# write_csv(output, 'data/output/m_boosting_xgb.csv')

# model <- xgb.train(data = train, objective = 'binary:logistic',
#                    nrounds = 1, num_parallel_tree = 1500, colsample_bytree = 0.25)
# pred <- as.numeric(predict(model, test) > 0.5)
# output <- bind_cols(select(dtest, PassengerId), data_frame(Survived = pred))
# write_csv(output, 'data/output/m_random_forest_xgb.csv')


