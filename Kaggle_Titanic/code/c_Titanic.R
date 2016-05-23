#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')

#=================================================================================
# read & clean data

train <- read_csv('data/input/train.csv') 
test <- read_csv('data/input/test.csv')

d_all <- 
  bind_rows(
  	train %>% 
  	  mutate(dataset = 'train'),
  	test %>% 
  	  mutate(dataset = 'test',
  	  	     Survived = NA)
  	)

#=================================================================================
# judgement-based imputation

d_all[d_all$PassengerId == 1044,]$Fare <- 7.8958
d_all[d_all$PassengerId %in% c(62, 830),]$Embarked <- 'C'

#=================================================================================
# feature engineering

#---------------------------------------------------------------------------------
# splitting features

d_all$LastName <- str_split_fixed(d_all$Name, ',', 2)[,1]
d_all$FirstName <- str_split_fixed(d_all$Name, ',', 2)[,2]
d_all$Title <- str_trim(str_split_fixed(d_all$FirstName, '\\.', 2)[,1])
d_all$Cabin <- str_sub(d_all$Cabin, 1, 1)
d_all$Ticket_A <- str_replace_all(str_split_fixed(d_all$Ticket, ' ', 2)[,1], '[0-9]+|\\.|/', '')
# d_all$Ticket_N <- str_extract(d_all$Ticket, '[0-9]+')
d_all[is.na(d_all)] <- -1
d_all <- 
  d_all %>%
  mutate(Survived = ifelse(Survived == -1, NA, Survived)) %>%
  mutate(Family = ifelse(SibSp == 0 & Parch == 0, 0, 1)) %>%
  group_by(LastName, Embarked, Pclass, Family) %>%
  mutate(n_LastName = n()) %>%
  # group_by(Ticket_N) %>%
  group_by(Ticket) %>%
  mutate(n_Ticket_N = n()) %>%
  ungroup() 

#---------------------------------------------------------------------------------
# grouping Titles

# table(d_all$Title)
d_all[d_all$Title %in% c('Mlle', 'Mme', 'Ms', 'the Countess', 'Lady', 'Ms', 'Dona'), ]$Title <- 'F'
d_all[d_all$Title %in% c('Capt', 'Rev'), ]$Title <- 'O'
d_all[d_all$Title %in% c('Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Sir', 'Master'), ]$Title <- 'M'

#---------------------------------------------------------------------------------
# save
save(d_all, file = 'data/working/Titanic_temp.RData')

#---------------------------------------------------------------------------------
# adding new features

d_all <- 
  d_all %>%
  # select(dataset, PassengerId, 
  #               Survived, Sex, Age, Pclass, Fare, Cabin, Embarked, Title, 
  #               SibSp, Parch, Family, Ticket_A, n_LastName, n_Ticket_N)
  mutate(Mother = ifelse(
    (Sex == 'female') & (Age > 18) & (Parch != 0) & (Title %in% c('F', 'Mrs')), 
    1, 0)
  ) %>%
  mutate(Children = ifelse(Age <= 18, 1, 0)) %>%
  select(dataset, PassengerId, 
                Survived, Sex, Age, Pclass, Fare, Cabin, Embarked, Title, 
                SibSp, Parch, Family, Ticket_A, n_LastName, n_Ticket_N,
                Mother, Children)

#=================================================================================
# model-based imputation

d_all_temp <-
  d_all %>%
  mutate(Sex = ifelse(Sex == 'male', 1, 0)) %>%
  select(PassengerId, Pclass, Sex, Age, SibSp, Parch, Fare, Family, n_LastName, n_Ticket_N)

# catagorical variable
dummy <- dummyVars(~ Title, data = d_all, fullRank = TRUE)
data_dummy <- tbl_df(data.frame(predict(dummy, d_all)))

d_all_age <- bind_cols(d_all_temp, data_dummy)

dtrain <-
  d_all_age %>% 
  filter(Age != -1) %>%
  select(-PassengerId)

dtest <-
  d_all_age %>% 
  filter(Age == -1) %>%
  select(-Age)

model <- randomForest(Age ~., data = dtrain, mtry = 3, ntree = 5000)
pred <- predict(model, select(dtest, -PassengerId))
output <- bind_cols(select(dtest, PassengerId), data_frame(Age_pred = pred))

d_all <-
  d_all %>%
  left_join(output, 'PassengerId') %>%
  mutate(Age = ifelse(Age == -1, Age_pred, Age)) %>%
  select(-Age_pred)

#=================================================================================
# save
save(d_all, file = 'data/working/Titanic.RData')
