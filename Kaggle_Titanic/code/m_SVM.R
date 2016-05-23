#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic.RData')

#=================================================================================
# data setup

# modify / select features so that they are appropriate for SVM 
d_all_temp <-
  d_all %>%
  mutate(Age = scale(Age),
         Fare = scale(Fare),
         Sex = ifelse(Sex == 'male', 1, 0),
         Survived = as.factor(Survived)) %>%
  # remove catagorical variables with too many catagories
  select(-c(Embarked, Cabin, Title, Ticket_A))

# catagorical variable
dummy <- dummyVars(~ Embarked + Cabin + Title + Ticket_A, data = d_all, fullRank = TRUE)
data_dummy <- tbl_df(data.frame(predict(dummy, d_all)))

d_all_mod <- bind_cols(d_all_temp, data_dummy)

#=================================================================================
# SVM

dtrain <-
  d_all_mod %>% 
  filter(dataset == 'train') %>%
  select(-dataset, -PassengerId)


dtest <-
  d_all_mod %>% 
  filter(dataset == 'test') %>%
  select(-dataset, -Survived)

#---------------------------------------------------------------------------------
# cross-validation

n_cv <- 10
set.seed(94305)
cv <- sample(1:n_cv, nrow(dtrain), replace = T)

results <- data_frame()
for (i in 1:n_cv) {

  model.radial <- svm(Survived ~., data = dtrain[cv!=i,], kernel = 'radial')
  pred.radial <- predict(model.radial, dtrain[cv==i, ])

  model.linear <- svm(Survived ~., data = dtrain[cv!=i,], kernel = 'linear')
  pred.linear <- predict(model.linear, dtrain[cv==i, ])

  model.polynomial <- svm(Survived ~., data = dtrain[cv!=i,], kernel = 'polynomial')
  pred.polynomial <- predict(model.polynomial, dtrain[cv==i, ])

  model.sigmoid <- svm(Survived ~., data = dtrain[cv!=i,], kernel = 'sigmoid')
  pred.sigmoid <- predict(model.sigmoid, dtrain[cv==i, ])

  results <- bind_rows(results, 
                       data_frame(Survived = dtrain[cv==i, ]$Survived, 
                                  pred.radial = pred.radial, 
                                  pred.linear = pred.linear, 
                                  pred.polynomial = pred.polynomial, 
                                  pred.sigmoid = pred.sigmoid, 
                                  cv = i))

}

results %>% 
group_by(cv) %>%
summarise(correct.radial = sum(Survived == pred.radial)/n(),
          correct.linear = sum(Survived == pred.linear)/n(),
          correct.polynomial = sum(Survived == pred.polynomial)/n(),
          correct.sigmoid = sum(Survived == pred.sigmoid)/n()) %>%
summarise(mean(correct.radial), sd(correct.radial),
          mean(correct.linear), sd(correct.linear),
          mean(correct.polynomial), sd(correct.polynomial),
          mean(correct.sigmoid), sd(correct.sigmoid))

# #---------------------------------------------------------------------------------
# # result

# model <- svm(Survived ~., data = dtrain, kernel = 'radial')
# pred <- predict(model, select(dtest, -PassengerId))
# output <- bind_cols(select(dtest, PassengerId), data_frame(Survived = pred))
# write_csv(output, 'data/output/m_SVM.csv')
