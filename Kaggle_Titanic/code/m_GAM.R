#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic.RData')

#=================================================================================
# data setup

# modify / select features so that they are appropriate for GAM 
d_all_temp <-
  d_all %>%
  mutate(Age = scale(Age),
         Fare = scale(Fare),
         Sex = ifelse(Sex == 'male', 1, 0)) %>%
  # remove catagorical variables with too many catagories
  select(-c(Embarked, Cabin, Title, Ticket_A))

# catagorical variable
dummy <- dummyVars(~ Embarked + Cabin + Title + Ticket_A, data = d_all, fullRank = TRUE)
data_dummy <- tbl_df(data.frame(predict(dummy, d_all)))

d_all_mod <- bind_cols(d_all_temp, data_dummy)

#=================================================================================
# GAM

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

  model <- gam(Survived ~., data = dtrain[cv!=i,], family = 'binomial')
  pred <- as.numeric(predict(model, dtrain[cv==i, ]) > 0.5)

  results <- bind_rows(results, data_frame(Survived = dtrain[cv==i, ]$Survived, pred = pred, cv = i))

}

results %>% 
group_by(cv) %>%
summarise(correct = sum(Survived == pred)/n()) %>%
summarise(mean(correct), sd(correct))

#---------------------------------------------------------------------------------
# result

model <- gam(Survived ~., data = dtrain, family = 'binomial')
pred <- as.numeric(predict(model, dtest) > 0.5)
output <- bind_cols(select(dtest, PassengerId), data_frame(Survived = pred))
write_csv(output, 'data/output/m_GAM.csv')
