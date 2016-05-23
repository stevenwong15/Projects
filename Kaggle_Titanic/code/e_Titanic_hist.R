#=================================================================================
# steven wong, february 2016
#=================================================================================
# setup
rm(list = ls())
source('~/Desktop/Analytics/Titanic/code/starter/project_library.R')
load('data/working/Titanic.RData')

#=================================================================================
# exploratory data analysis - histograms

#---------------------------------------------------------------------------------
# test vs. train

# Pclass
d_all %>%
group_by(dataset, Pclass) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(Pclass), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Class', y = '# Passengers')
save_as('graphics/Pclass.pdf', 5, 10)

# Sex
d_all %>%
group_by(dataset, Sex) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(Sex), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Sex', y = '# Passengers')
save_as('graphics/Sex.pdf', 5, 10)

# SibSp
d_all %>%
group_by(dataset, SibSp) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(SibSp), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Number of Siblings / Spouses Aboard', y = '# Passengers')
save_as('graphics/SibSp.pdf', 5, 10)

# Parch
d_all %>%
group_by(dataset, Parch) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(Parch), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Number of Parents / Children Aboard', y = '# Passengers')
save_as('graphics/Parch.pdf', 5, 10)

# Embarked
d_all %>%
group_by(dataset, Embarked) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(Embarked), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Port of Embarkation', y = '# Passengers')
save_as('graphics/Embarked.pdf', 5, 10)

# Age
d_all %>%
mutate(Age = round(Age/5)*5) %>%
group_by(dataset, Age) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>% 
  ggplot(aes(x = factor(Age), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Age (Bucket to Nearest 10)', y = '# Passengers')
save_as('graphics/Age.pdf', 5, 20)

# Fare
d_all %>%
mutate(Fare = round(Fare/10)*10) %>%
group_by(dataset, Fare) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>% 
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(Fare), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Fare (Bucket to Nearest 20)', y = '# Passengers')
save_as('graphics/Fare.pdf', 5, 20)

# Title
d_all %>%
group_by(dataset, Title) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(Title), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Title', y = '# Passengers')
save_as('graphics/Title.pdf', 5, 20)

# Cabin
d_all %>%
group_by(dataset, Cabin) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(Cabin), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Cabin', y = '# Passengers')
save_as('graphics/Cabin.pdf', 5, 10)

# n_LastName
d_all %>%
group_by(dataset, n_LastName) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(n_LastName), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'n_LastName', y = '# Passengers')
save_as('graphics/n_LastName.pdf', 5, 10)

# Ticket_A
d_all %>%
group_by(dataset, Ticket_A) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(Ticket_A), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'Ticket_A', y = '# Passengers')
save_as('graphics/Ticket_A.pdf', 5, 20)

# n_Ticket_N
d_all %>%
group_by(dataset, n_Ticket_N) %>%
summarise(nPassenger = n()) %>%
group_by(dataset) %>%
mutate(nPassenger_Per = nPassenger / sum(nPassenger)) %>%
  ggplot(aes(x = factor(n_Ticket_N), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~dataset) + 
  labs(x = 'n_Ticket_N', y = '# Passengers')
save_as('graphics/n_Ticket_N.pdf', 5, 20)

#---------------------------------------------------------------------------------
# train: individual variable vs. Survived

# Pclass
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, Pclass) %>%
summarise(nPassenger = n()) %>%
group_by(Pclass) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(Pclass), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Class', y = '# Passengers')
save_as('graphics/Survived_Pclass.pdf', 5, 10)

# Sex
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, Sex) %>%
summarise(nPassenger = n()) %>%
group_by(Sex) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(Sex), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Sex', y = '# Passengers')
save_as('graphics/Survived_Sex.pdf', 5, 10)

# SibSp
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, SibSp) %>%
summarise(nPassenger = n()) %>%
group_by(SibSp) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(SibSp), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Number of Siblings / Spouses Aboard', y = '# Passengers')
save_as('graphics/Survived_SibSp.pdf', 5, 10)

# Parch
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, Parch) %>%
summarise(nPassenger = n()) %>%
group_by(Parch) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(Parch), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Number of Parents / Children Aboard', y = '# Passengers')
save_as('graphics/Survived_Parch.pdf', 5, 10)

# Embarked
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, Embarked) %>%
summarise(nPassenger = n()) %>%
group_by(Embarked) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(Embarked), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Port of Embarkation', y = '# Passengers')
save_as('graphics/Survived_Embarked.pdf', 5, 10)

# Age
d_all %>%
filter(dataset == 'train') %>%
mutate(Age = round(Age/5)*5) %>%
group_by(Survived, Age) %>%
summarise(nPassenger = n()) %>%
group_by(Age) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(Age), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Age (Bucket to Nearest 5)', y = '# Passengers')
save_as('graphics/Survived_Age.pdf', 5, 20)

# Fare
d_all %>%
filter(dataset == 'train') %>%
mutate(Fare = round(Fare/10)*10) %>%
group_by(Survived, Fare) %>%
summarise(nPassenger = n()) %>%
group_by(Fare) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(Fare), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Fare (Bucket to Nearest 10)', y = '# Passengers')
save_as('graphics/Survived_Fare.pdf', 5, 20)

# Title
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, Title) %>%
summarise(nPassenger = n()) %>%
group_by(Title) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(Title), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Title', y = '# Passengers')
save_as('graphics/Survived_Title.pdf', 5, 20)

# Cabin
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, Cabin) %>%
summarise(nPassenger = n()) %>%
group_by(Cabin) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(Cabin), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Cabin', y = '# Passengers')
save_as('graphics/Survived_Cabin.pdf', 5, 10)

# n_LastName
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, n_LastName) %>%
summarise(nPassenger = n()) %>%
group_by(n_LastName) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(n_LastName), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'n_LastName', y = '# Passengers')
save_as('graphics/Survived_n_LastName.pdf', 5, 10)

# Ticket_A
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, Ticket_A) %>%
summarise(nPassenger = n()) %>%
group_by(Ticket_A) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(Ticket_A), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'Ticket_A', y = '# Passengers')
save_as('graphics/Survived_Ticket_A.pdf', 5, 20)

# n_Ticket_N
d_all %>%
filter(dataset == 'train') %>%
group_by(Survived, n_Ticket_N) %>%
summarise(nPassenger = n()) %>%
group_by(n_Ticket_N) %>%
mutate(nPassenger_Per = nPassenger/sum(nPassenger)) %>%
  ggplot(aes(x = factor(n_Ticket_N), y = nPassenger)) + 
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(y = 0, label = str_c(round(nPassenger_Per*100, 1), '%')), vjust=-0.1) + 
  facet_wrap(~Survived) + 
  labs(x = 'n_Ticket_N', y = '# Passengers')
save_as('graphics/Survived_n_Ticket_N.pdf', 5, 20)
