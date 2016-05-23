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
d_all_mod <-
  d_all %>%
  mutate(Age = scale(Age),
         Fare = scale(Fare)) %>%
  # convert catagorical variables to factors, for naiveBayes()
  mutate(
    Survived = as.factor(Survived),
    Sex = as.factor(Sex),
    Pclass = as.factor(Pclass),
    Cabin = as.factor(Cabin),
    Embarked = as.factor(Embarked),
    Title = as.factor(Title),
    Ticket_A = as.factor(Ticket_A))

#=================================================================================
# naive bayes

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
# laplace smoothing (k = strength of the prior)

laplace <- 0:10

n_cv <- 10
set.seed(94305)
cv <- sample(1:n_cv, nrow(dtrain), replace = T)

results <- data_frame()
for(k in 1:length(laplace)) {
for (i in 1:n_cv) {

  model <- naiveBayes(Survived ~., data = dtrain[cv!=i,], laplace = laplace[k])
  pred <- predict(model, dtrain[cv==i, ])

  results <- bind_rows(
    results, 
    data_frame(
      Survived = dtrain[cv==i, ]$Survived, 
      pred = pred, 
      cv = i, laplace = laplace[k])
  )

}}

results %>% 
group_by(cv, laplace) %>%
summarise(correct = sum(Survived == pred)/n()) %>%
group_by(laplace) %>%
summarise(mean(correct), sd(correct))

#---------------------------------------------------------------------------------
# result

model <- naiveBayes(Survived ~., data = dtrain, laplace = 0)
pred <- predict(model, select(dtest, -PassengerId))
output <- bind_cols(select(dtest, PassengerId), data_frame(Survived = pred))
write_csv(output, 'data/output/m_naive_bayes.csv')
