#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic_temp.RData')

#=================================================================================
# data setup

#---------------------------------------------------------------------------------
# missing values

table(d_all$Age)  #263
table(d_all$Fare)  #1
table(d_all$Cabin)  #1014
table(d_all$Embarked)  #2

#=================================================================================
# judgment-based imputation

# Fare
d_all %>% filter(Fare == -1)
# similar profile
d_all_ <- 
  d_all %>%
  filter(Sex == 'male',
         Pclass == 3,
         SibSp == 0,
         Parch == 0,
         Embarked == 'S',
         Title == 'Mr')
mean(d_all_$Fare)  # 9.616241
median(d_all_$Fare)  # 7.8958
ggplot(d_all_, aes(x = Fare)) +
  geom_histogram(bins = 50)

# Embarked
d_all %>% filter(Embarked == -1)
# most likely C
ggplot(d_all, aes(x = factor(Pclass), y = Fare)) + 
  geom_boxplot(alpha = 0.1) + 
  facet_grid(~Embarked)

#=================================================================================
# model-based imputation

#---------------------------------------------------------------------------------
# train/test set to model age

d_all_temp <-
  d_all %>%
  mutate(Sex = ifelse(Sex == 'male', 1, 0)) %>%
  select(PassengerId, Pclass, Sex, Age, SibSp, Parch, Fare, Family, n_LastName, n_Ticket_N)

# # age correlates with Pclass, SibSp, Fare
# cor(d_all_temp[,-1])

# catagorical variable
dummy <- dummyVars(~ Title + Embarked, data = d_all, fullRank = TRUE)
data_dummy <- tbl_df(data.frame(predict(dummy, d_all)))

d_all_age <- bind_cols(d_all_temp, data_dummy)

# # age correlates with Pclass, SibSp, Fare
# cor(d_all_age[,-1])

dtrain <-
  d_all_age %>% 
  filter(Age != -1) %>%
  select(-PassengerId)

dtest <-
  d_all_age %>% 
  filter(Age == -1) %>%
  select(-Age)

#---------------------------------------------------------------------------------
# regression

n_cv <- 10
set.seed(94305)
cv <- sample(1:n_cv, nrow(dtrain), replace = T)

results <- data_frame()
for (i in 1:n_cv) {

  model <- glm(Age ~., data = dtrain[cv!=i,], family = gaussian)
  pred.lr <- predict(model, dtrain[cv==i, ])

  set.seed(94305)
  cv.lasso <- cv.glmnet(as.matrix(select(dtrain[cv!=i, ], -Age)), dtrain[cv!=i, ]$Age, 
                        family = 'gaussian', alpha = 1, nfold = 10)
  pred.lasso <- as.numeric(predict(cv.lasso, as.matrix(select(dtrain[cv==i,], -Age)), s='lambda.min'))

  set.seed(94305)
  cv.ridge <- cv.glmnet(as.matrix(select(dtrain[cv!=i, ], -Age)), dtrain[cv!=i, ]$Age, 
                        family = 'gaussian', alpha = 0, nfold = 10)
  pred.ridge <- as.numeric(predict(cv.ridge, as.matrix(select(dtrain[cv==i,], -Age)), s='lambda.min'))

  results <- bind_rows(results, 
                       data_frame(Age = dtrain[cv==i, ]$Age, 
                                  pred.lr = pred.lr, 
                                  pred.lasso = pred.lasso, 
                                  pred.ridge = pred.ridge, 
                                  cv = i))

}

#   mean(rss.lr) sd(rss.lr) mean(rss.lasso) sd(rss.lasso) mean(rss.ridge) sd(rss.ridge)
#          (dbl)      (dbl)           (dbl)         (dbl)           (dbl)         (dbl)
# 1     130.9479   18.72932        130.5571      18.40028        130.1173      19.19485
results %>% 
group_by(cv) %>%
summarise(rss.lr = mean((Age - pred.lr)^2),
          rss.lasso = mean((Age - pred.lasso)^2),
          rss.ridge = mean((Age - pred.ridge)^2)) %>%
summarise(mean(rss.lr), sd(rss.lr),
          mean(rss.lasso), sd(rss.lasso),
          mean(rss.ridge), sd(rss.ridge))

#---------------------------------------------------------------------------------
# random forest

# # importance
# model <- randomForest(Age ~., data = dtrain, importance = TRUE, mtry = 3, ntree = 5000)
# importance(model)
# varImpPlot(model)
# plot(model)

n_cv <- 10
set.seed(94305)
cv <- sample(1:n_cv, nrow(dtrain), replace = T)

# n_tree <- c(seq(0, 10, 2.5)*1e2)[-1]
n_tree <- c(2000)
n_p <- 1:5

results <- data_frame()
for (p in 1:length(n_p)) {
for (t in 1:length(n_tree)) {
for (i in 1:n_cv) {

  model <- randomForest(Age ~., data = dtrain[cv!=i,], ntree = n_tree[t], mtry = n_p[p])
  pred <- predict(model, dtrain[cv==i, ])

  results <- bind_rows(results, 
                       data_frame(Age = dtrain[cv==i, ]$Age, 
                                  pred = pred, 
                                  cv = i,
                                  n_tree = n_tree[t],
                                  n_p = n_p[p]))

}}}

# 500 trees
#      n_p n_tree mean(rss)  sd(rss)
#    (int)  (dbl)     (dbl)    (dbl)
# 1      1    500  141.9591 25.30457
# 2      2    500  116.8987 17.30863
# 3      3    500  113.0131 15.79734 <-
# 4      4    500  113.6994 15.98190
# 5      5    500  114.7998 15.89089
# 6      6    500  115.6681 15.51755
# 7      7    500  117.6341 16.61647
# 8      8    500  119.3614 16.68487
# 9      9    500  120.5163 17.07248
# 10    10    500  121.9948 17.16904
# 2000 trees
#     n_p n_tree mean(rss)  sd(rss)
#   (int)  (dbl)     (dbl)    (dbl)
# 1     1   2000  141.6878 25.49396
# 2     2   2000  116.6786 17.53603
# 3     3   2000  112.7598 16.16656 <-
# 4     4   2000  113.2073 15.76398
# 5     5   2000  114.4775 16.09176
results %>% 
group_by(cv, n_tree, n_p) %>%
summarise(rss = mean((Age - pred)^2)) %>%
group_by(n_p, n_tree) %>%
summarise(mean(rss), sd(rss))
