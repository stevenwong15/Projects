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
dummy <- dummyVars(~ Embarked, data = d_all, fullRank = TRUE)
data_dummy <- tbl_df(data.frame(predict(dummy, d_all)))

d_all_mod <- bind_cols(d_all_temp, data_dummy)

#=================================================================================
# linear / quadratic discriminant analysis

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

  model <- lda(Survived ~., data = dtrain[cv!=i,])
  # model <- qda(Survived ~., data = dtrain[cv!=i,])
  pred <- predict(model, dtrain[cv==i, ])$class

  results <- bind_rows(results, data_frame(Survived = dtrain[cv==i, ]$Survived, pred = pred, cv = i))

}

results %>% 
group_by(cv) %>%
summarise(correct = sum(Survived == pred)/n()) %>%
summarise(mean(correct), sd(correct))

#---------------------------------------------------------------------------------
# result

model <- lda(Survived ~., data = dtrain)
pred <- predict(model, dtest)$class
output <- bind_cols(select(dtest, PassengerId), data_frame(Survived = pred))
write_csv(output, 'data/output/m_linear_discriminant_analysis.csv')
