#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic.RData')

#=================================================================================
# data setup

# modify / select features so that they are appropriate for logistic regression 
d_all_temp <-
  d_all %>%
  ungroup() %>%
  mutate(Age = scale(Age),
  	     Fare = scale(Fare),
  	     Sex = ifelse(Sex == 'male', 1, 0)) %>%
  select(-c(Title, Ticket_A, Cabin, Embarked))

# catagorical variable
dummy <- dummyVars(~ Embarked + Title + Ticket_A + Cabin, data = d_all, fullRank = TRUE)
data_dummy <- tbl_df(data.frame(predict(dummy, d_all)))

d_all_mod <- bind_cols(d_all_temp, data_dummy)

#=================================================================================
# logistic regression

# to run faster for more iterations
# d_pca <- 
#   d_all_mod %>%
#   select(-c(PassengerId, dataset, Survived))

# pca.out <- prcomp(d_pca, scale = T)

# dtrain <- data.frame(pca.out$x[d_all_mod$dataset == 'train', 1:10], 
#                      Survived = d_all_mod$Survived[d_all_mod$dataset == 'train'])
# dtest <- pca.out$x[d_all_mod$dataset == 'test', 1:10]

dtrain <-
  d_all_mod %>% 
  filter(dataset == 'train') %>%
  select(-dataset, -PassengerId)

dtest <-
  d_all_mod %>% 
  filter(dataset == 'test') %>%
  select(-dataset, -Survived)

# #---------------------------------------------------------------------------------
# # lasso

# set.seed(94305)
# cv.lasso <- cv.glmnet(as.matrix(select(dtrain, -Survived)), dtrain$Survived, 
#                       family = 'binomial', alpha = 1, nfold = 10)
# # plot(cv.lasso)
# # coef(cv.lasso)
# predict(cv.lasso, as.matrix(select(dtrain, -Survived)), s='lambda.1se')

# #---------------------------------------------------------------------------------
# # ridge

# set.seed(94305)
# cv.ridge <- cv.glmnet(as.matrix(select(dtrain, -Survived)), dtrain$Survived,
#                       family = 'binomial', alpha = 0, nfold = 10)
# # plot(cv.ridge)
# # coef(cv.ridge)
# predict(cv.ridge, as.matrix(select(dtrain, -Survived)), s='lambda.1se')

#---------------------------------------------------------------------------------
# cross-validation

n_cv <- 10
set.seed(94305)
cv <- sample(1:n_cv, nrow(dtrain), replace = T)

results <- data_frame()
for (i in 1:n_cv) {

  model <- glm(Survived ~., data = dtrain[cv!=i,], family = binomial)
  pred.lr <- as.numeric(predict(model, dtrain[cv==i, ]) > 0.5)

  set.seed(94305)
  cv.lasso <- cv.glmnet(as.matrix(select(dtrain[cv!=i, ], -Survived)), dtrain[cv!=i, ]$Survived, 
                        family = 'binomial', alpha = 1, nfold = 10)
  pred.lasso <- as.numeric(predict(cv.lasso, as.matrix(select(dtrain[cv==i,], -Survived)), s='lambda.min') > 0.5)

  set.seed(94305)
  cv.ridge <- cv.glmnet(as.matrix(select(dtrain[cv!=i, ], -Survived)), dtrain[cv!=i, ]$Survived, 
                        family = 'binomial', alpha = 0, nfold = 10)
  pred.ridge <- as.numeric(predict(cv.ridge, as.matrix(select(dtrain[cv==i,], -Survived)), s='lambda.min') > 0.5)

  results <- bind_rows(results, 
                       data_frame(Survived = dtrain[cv==i, ]$Survived, 
                                  pred.lr = pred.lr, 
                                  pred.lasso = pred.lasso, 
                                  pred.ridge = pred.ridge, 
                                  cv = i))

}

results %>% 
group_by(cv) %>%
summarise(correct.lr = sum(Survived == pred.lr)/n(),
          correct.lasso = sum(Survived == pred.lasso)/n(),
          correct.ridge = sum(Survived == pred.ridge)/n()) %>%
summarise(mean(correct.lr), sd(correct.lr),
          mean(correct.lasso), sd(correct.lasso),
          mean(correct.ridge), sd(correct.ridge))

# #---------------------------------------------------------------------------------
# # result

# model <- cv.glmnet(as.matrix(select(dtrain, -Survived)), dtrain$Survived, family = 'binomial', alpha = 1, nfold = 10)
# pred <- as.numeric(predict(model, as.matrix(select(dtest, -PassengerId)), s='lambda.min') > 0.5)
# output <- bind_cols(select(dtest, PassengerId), data_frame(Survived = pred))
# write_csv(output, 'data/output/m_logistic_lasso.csv')

# model <- cv.glmnet(as.matrix(select(dtrain, -Survived)), dtrain$Survived, family = 'binomial', alpha = 0, nfold = 10)
# pred <- as.numeric(predict(model, as.matrix(select(dtest, -PassengerId)), s='lambda.min') > 0.5)
# output <- bind_cols(select(dtest, PassengerId), data_frame(Survived = pred))
# write_csv(output, 'data/output/m_logistic_ridge.csv')

