#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic.RData')

#=================================================================================
# data setup

# modify / select features so that they are appropriate for random forest
d_all_temp <-
  d_all %>%
  mutate(Sex = ifelse(Sex == 'male', 1, 0),
         Survived = as.factor(Survived)) %>%
  # remove catagorical variables with too many catagories
  select(-c(Embarked, Cabin, Title, Ticket_A))

# catagorical variable
dummy <- dummyVars(~ Embarked + Cabin + Title + Ticket_A, data = d_all, fullRank = TRUE)
data_dummy <- tbl_df(data.frame(predict(dummy, d_all)))

d_all_mod <- bind_cols(d_all_temp, data_dummy)

#=================================================================================
# random forest

dtrain <-
  d_all_mod %>% 
  filter(dataset == 'train') %>%
  select(-dataset, -PassengerId)

dtest <-
  d_all_mod %>% 
  filter(dataset == 'test') %>%
  select(-dataset, -Survived)

# #---------------------------------------------------------------------------------
# # importance

# model <- randomForest(Survived ~., data = dtrain, importance = TRUE, mtry = 9, ntree = 2500)
# importance(model)
# varImpPlot(model)
# plot(model)

#---------------------------------------------------------------------------------
# cross-validation

n_cv <- 10
set.seed(94305)
cv <- sample(1:n_cv, nrow(dtrain), replace = T)

# n_tree <- c(seq(0, 10, 2.5)*1e2)[-1]
n_tree <- c(5000)
n_p <- 5:8

results <- data_frame()
for (p in 1:length(n_p)) {
for (t in 1:length(n_tree)) {
for (i in 1:n_cv) {

  model <- randomForest(Survived ~., data = dtrain[cv!=i,], ntree = n_tree[t], mtry = n_p[p])
  pred <- predict(model, dtrain[cv==i, ])

  results <- bind_rows(
    results, 
    data_frame(
      Survived = dtrain[cv==i, ]$Survived, 
      pred = pred, 
      cv = i,
      n_tree = n_tree[t],
      n_p = n_p[p])
  )

}}}

results %>% 
group_by(cv, n_tree, n_p) %>%
summarise(correct = sum(Survived == pred)/n()) %>%
group_by(n_p, n_tree) %>%
summarise(mean(correct), sd(correct))

# #---------------------------------------------------------------------------------
# # result

# model <- randomForest(Survived ~., data = dtrain, mtry = 5, ntree = 5000)
# pred <- predict(model, select(dtest, -PassengerId))
# output <- bind_cols(select(dtest, PassengerId), data_frame(Survived = pred))
# write_csv(output, 'data/output/m_random_forest.csv')
