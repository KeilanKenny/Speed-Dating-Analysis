# ----------------------------------------
# Analyse Data
# ----------------------------------------
# This script processes raw data:
# 1. Participants Demographics
# 2. Participants Choices
# 3. Correlation
# ----------------------------------------

# Load necessary libraries

# ----------------------------------------
# Step 1: Participants Demographics
# ----------------------------------------

# Age 
summary(data$age)

data_age <- data %>% group_by(iid, age, gender) %>% summarise(age = max(age), .groups = 'drop')
data_age$gender <- with(data_age, ifelse(data_age$gender == 0, 'Female', 'Male'))


data_age %>% group_by(gender) %>% summarise(	n(), .groups = 'drop')

ggplot(data_age, aes(x = age, fill = gender)) + 
  geom_histogram(data = subset(data_age, gender == "Female"), aes(y = ..count.. * (-1)), binwidth = 1, color = "black") +  
  geom_histogram(data = subset(data_age, gender == "Male"), binwidth = 1, color = "black") + 
  scale_y_continuous(breaks = seq(-70, 70, 10), labels = abs(seq(-70, 70, 10)))+ 
  scale_x_continuous(breaks = seq(5, 55, 5), labels = seq(5, 55, 5)) + 
  scale_fill_discrete("Gender", labels = c("Female", "Male"))+
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  coord_flip()


# Race
data_race <- na.omit(data %>% group_by(iid, race, gender) %>% summarise(.groups = 'drop'))

data_race$gender <- with(data_race, ifelse(data_race$gender == 0, 'Female', 'Male'))

data_race$race <- with(data_race, ifelse(data_race$race == 1, 'Black/African American', 
                                         ifelse(data_race$race == 2, 'European/Caucasian-American',
                                                ifelse(data_race$race == 3, 'Latino/Hispanic American',
                                                       ifelse(data_race$race == 4, 'Asian/Pacific Islander/Asian-American', 
                                                              ifelse(data_race$race == 5, 'Native American', 'Other'))))))

data_race %>% group_by(race, gender) %>% summarise(	n(), .groups = 'drop')

ggplot(data_race) +
  geom_bar(position="dodge",aes(race, fill=gender), color = "black") + 
  scale_fill_discrete("Gender", labels = c("Female", "Male")) +
  labs(title = "Race Distribution", x = "Race", y = "Count") +
  coord_flip()

# Career
data_career <- na.omit(data %>%
                         group_by(iid, career_c, gender) %>%
                         summarise(.groups = 'drop'))

# Recode gender
data_career$gender <- with(data_career, ifelse(gender == 0, 'Female', 'Male'))

# Check unique values for career_c
unique(data_career$career_c)

# Recode career_c
data_career$career_c <- with(data_career, 
                             ifelse(career_c == 1, 'Lawyer', 
                                    ifelse(career_c == 2, 'Academic/Research', 
                                           ifelse(career_c == 3, 'Psychologist', 
                                                  ifelse(career_c == 4, 'Doctor/Medicine', 
                                                         ifelse(career_c == 5, 'Engineer', 
                                                                ifelse(career_c == 6, 'Creative Arts/Entertainment', 
                                                                       ifelse(career_c == 7, 'Banking/Consulting/Finance/Marketing/Business/CEO/Entrepreneur/Admin', 
                                                                              ifelse(career_c == 8, 'Real Estate', 
                                                                                     ifelse(career_c == 9, 'International/Humanitarian Affairs', 
                                                                                            ifelse(career_c == 10, 'Undecided', 
                                                                                                   ifelse(career_c == 11, 'Social Work', 
                                                                                                          ifelse(career_c == 12, 'Speech Pathology', 
                                                                                                                 ifelse(career_c == 13, 'Politics', 
                                                                                                                        ifelse(career_c == 14, 'Pro sports/Athletics', 
                                                                                                                               ifelse(career_c == 15, 'Other', 
                                                                                                                                      ifelse(career_c == 16, 'Journalism', 'Architecture')))))))))))))))))

# Check unique values for career_c again
unique(data_career$career_c)

# Plot career distribution by gender
ggplot(data_career) +
  geom_bar(position = "dodge", aes(career_c, fill = gender), color = "black") +
  scale_fill_discrete("Gender", labels = c("Female", "Male")) +
  labs(title = "Career Distribution", x = "Career", y = "Count") +
  coord_flip()


# ----------------------------------------
# Step 2: Participants Choices
# ----------------------------------------

# Goals
data_goals <- na.omit(data %>% group_by(iid, gender, goal) %>% summarise(.groups = 'drop'))
data_goals$gender <- with(data_goals, ifelse(data_goals$gender == 0, 'Female', 'Male'))

data_goals$goal <- with(data_goals, ifelse(data_goals$goal == 1, 'Seemed like a fun night out', 
                                           ifelse(data_goals$goal == 2, 'To meet new people',
                                                  ifelse(data_goals$goal == 3, 'To get a date',
                                                         ifelse(data_goals$goal == 4, 'Looking for a serious relationship',
                                                                ifelse(data_goals$goal == 5, 'To say I did it', 'Other'))))))


ggplot(data_goals,aes(goal, fill=gender)) +
  geom_bar(position="dodge", color = "black") + 
  scale_fill_discrete("Gender", labels = c("Female", "Male")) +
  labs(title = "Goal Distribution", x = "Career", y = "Count") +
  coord_flip()





## attributes
### what males & females are looking for
data_attributes <- na.omit(data %>% group_by(iid, gender, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1) %>% summarise(.groups = 'drop'))
data_attributes$gender <- with(data_attributes, ifelse(data_attributes$gender == 0, 'Female', 'Male'))
data_attributes$total <- rowSums(data_attributes[ , 3:8])

data_attributes$attr1_1_percent <- ((data_attributes$attr1_1/data_attributes$total)*100)
data_attributes$sinc1_1_percent <- ((data_attributes$sinc1_1/data_attributes$total)*100)
data_attributes$intel1_1_percent <- ((data_attributes$intel1_1/data_attributes$total)*100)
data_attributes$fun1_1_percent <- ((data_attributes$fun1_1/data_attributes$total)*100)
data_attributes$amb1_1_percent <- ((data_attributes$amb1_1/data_attributes$total)*100)
data_attributes$shar1_1_percent <- ((data_attributes$shar1_1/data_attributes$total)*100)

data_attributes$total_percent <- rowSums(data_attributes[ , 10:15])




data_attributes_plot <- subset(data_attributes, select=c('iid','gender','attr1_1_percent','sinc1_1_percent','intel1_1_percent','fun1_1_percent','amb1_1_percent','shar1_1_percent'))

data_attributes_plot <- data_attributes_plot %>% group_by(gender) %>% summarise(Attractive = mean(attr1_1_percent), 
                                                                                Sincere = mean(sinc1_1_percent), 
                                                                                Intelligent = mean(intel1_1_percent), 
                                                                                Fun = mean(fun1_1_percent), 
                                                                                Ambitious = mean(amb1_1_percent), 
                                                                                Interest = mean(shar1_1_percent))


data_attributes_plot <- t(data_attributes_plot)
data_attributes_plot <- data_attributes_plot[-1,]

data_attributes_plot<- cbind(newColName = rownames(data_attributes_plot), data_attributes_plot)
rownames(data_attributes_plot) <- 1:nrow(data_attributes_plot)
colnames(data_attributes_plot) <- c("Attribute", "Female","Male")
data_attributes_plot <- as.data.frame(data_attributes_plot)


data_attributes_plot %>%
  mutate(Attribute = fct_reorder(Attribute, Male)) %>%
  ggplot(aes(x=Attribute, y=Male)) +
  geom_bar(stat="identity", color = "black", fill='#00bfc4') +
  labs(title = "Desired Attributes for Males", x = "Attribute", y = "Count") +
  coord_flip()


ggplot(data_attributes_plot, aes(x=Attribute, y=Male)) +
  geom_bar(stat="identity", color = "black", fill='#00bfc4') +
  labs(title = "Desired Attributes for Males", x = "Attribute", y = "Count") +
  coord_flip()



ggplot(data_attributes_plot, aes(x=Attribute, y=Female)) +
  geom_bar(stat="identity", color = "black", fill='#f8766d') +
  labs(title = "Desired Attributes for Females", x = "Attribute", y = "Count") +
  coord_flip()

data_attributes_plot %>%
  mutate(Attribute = fct_reorder(Attribute, Female)) %>%
  ggplot(aes(x=Attribute, y=Female)) +
  geom_bar(stat="identity", color = "black", fill='#f8766d') +
  labs(title = "Desired Attributes for Females", x = "Attribute", y = "Count") +
  coord_flip()

### what males & females think the opposite sex are looking for
data_attributes <- na.omit(data %>% group_by(iid, gender, attr2_1, sinc2_1, intel2_1, fun2_1, amb2_1, shar2_1) %>% summarise(.groups = 'drop'))
data_attributes$gender <- with(data_attributes, ifelse(data_attributes$gender == 0, 'Female', 'Male'))
data_attributes$total <- rowSums(data_attributes[ , 3:8])

data_attributes$attr2_1_percent <- ((data_attributes$attr2_1/data_attributes$total)*100)
data_attributes$sinc2_1_percent <- ((data_attributes$sinc2_1/data_attributes$total)*100)
data_attributes$intel2_1_percent <- ((data_attributes$intel2_1/data_attributes$total)*100)
data_attributes$fun2_1_percent <- ((data_attributes$fun2_1/data_attributes$total)*100)
data_attributes$amb2_1_percent <- ((data_attributes$amb2_1/data_attributes$total)*100)
data_attributes$shar2_1_percent <- ((data_attributes$shar2_1/data_attributes$total)*100)

data_attributes$total_percent <- rowSums(data_attributes[ , 10:15])




data_attributes_plot <- subset(data_attributes, select=c('iid','gender','attr2_1_percent','sinc2_1_percent','intel2_1_percent','fun2_1_percent','amb2_1_percent','shar2_1_percent'))

data_attributes_plot <- data_attributes_plot %>% group_by(gender) %>% summarise(Attractive = mean(attr2_1_percent), 
                                                                                Sincere = mean(sinc2_1_percent), 
                                                                                Intelligent = mean(intel2_1_percent), 
                                                                                Fun = mean(fun2_1_percent), 
                                                                                Ambitious = mean(amb2_1_percent), 
                                                                                Interest = mean(shar2_1_percent))


data_attributes_plot <- t(data_attributes_plot)
data_attributes_plot <- data_attributes_plot[-1,]

data_attributes_plot <- cbind(newColName = rownames(data_attributes_plot), data_attributes_plot)
rownames(data_attributes_plot) <- 1:nrow(data_attributes_plot)
colnames(data_attributes_plot) <- c("Attribute", "Female","Male")
data_attributes_plot <- as.data.frame(data_attributes_plot)


data_attributes_plot %>%
  mutate(Attribute = fct_reorder(Attribute, Male)) %>%
  ggplot(aes(x=Attribute, y=Male)) +
  geom_bar(stat="identity", color = "black", fill='#00bfc4') +
  #scale_y_discrete(breaks=c(0,5,10,15,20.25))+
  #scale_y_discrete(breaks = seq(10, 30, 10), labels = abs(seq(10, 30, 10)))+
  labs(title = "Desired Attributes for Males, Female opinion.", x = "Attribute", y = "Count") +
  coord_flip()


ggplot(data_attributes_plot, aes(x=Attribute, y=Male)) +
  geom_bar(stat="identity", color = "black", fill='#00bfc4') +
  labs(title = "Desired Attributes for Males, Female opinion.", x = "Attribute", y = "Count") +
  coord_flip()



ggplot(data_attributes_plot, aes(x=Attribute, y=Female)) +
  geom_bar(stat="identity", color = "black", fill='#f8766d') +
  labs(title = "Desired Attributes for Females, Male opinion.", x = "Attribute", y = "Count") +
  coord_flip()

data_attributes_plot %>%
  mutate(Attribute = fct_reorder(Attribute, Female)) %>%
  ggplot(aes(x=Attribute, y=Female)) +
  geom_bar(stat="identity", color = "black", fill='#f8766d') +
  labs(title = "Desired Attributes for Females, Male opinion.", x = "Attribute", y = "Count") +
  coord_flip()

# ----------------------------------------
# Step 3: Correlation
# ----------------------------------------

data$pgender <- with(data, ifelse(data$gender == 0, 1, 0))
data_correlation <- data %>% group_by(pid, pgender) %>% summarise(Decision = mean(dec), 
                                                                  Attractive = mean(attr), 
                                                                  Sincere = mean(sinc), 
                                                                  Intelligent = mean(intel), 
                                                                  Fun = mean(fun), 
                                                                  Ambitious = mean(amb), 
                                                                  Interest = mean(shar))



test_attr <-
  data_correlation %>% 
  select(pid, pgender, Decision, Attractive)%>% 
  filter(!Attractive == "NA")
test_sinc<-
  data_correlation %>% 
  select(pid, pgender, Decision, Sincere) %>% 
  filter(!Sincere == "NA")

test_intel <-
  data_correlation %>% 
  select(pid, pgender, Decision, Intelligent) %>% 
  filter(!Intelligent == "NA")

test_fun <-
  data_correlation %>% 
  select(pid, pgender, Decision, Fun) %>% 
  filter(!Fun == "NA")

test_amb <-
  data_correlation %>% 
  select(pid, pgender, Decision, Ambitious) %>% 
  filter(!Ambitious == "NA")

test_shar <-
  data_correlation %>% 
  select(pid, pgender, Decision, Interest) %>% 
  filter(!Interest == "NA")

cor(test_attr$Decision, test_attr$Attractive)
cor(test_sinc$Decision, test_sinc$Sincere)
cor(test_intel$Decision, test_intel$Intelligent)
cor(test_fun$Decision, test_fun$Fun)
cor(test_amb$Decision, test_amb$Ambitious)
cor(test_shar$Decision, test_shar$Interest)

coratr <- round(cor(test_attr$Decision, test_attr$Attractive), digits = 3)
corsin <- cor(test_sinc$Decision, test_sinc$Sincere)
corint <- cor(test_intel$Decision, test_intel$Intelligent)
corfun <- cor(test_fun$Decision, test_fun$Fun)
coramb <- round(cor(test_amb$Decision, test_amb$Ambitious), digits = 3)
corshar <- cor(test_shar$Decision, test_shar$Interest)

test_attr %>% 
  ggplot() +
  aes(x = Decision, y = Attractive) +
  geom_point(alpha = 1.1) +
  geom_smooth(method = lm,
              se = FALSE, 
              size = 1.5)+
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 1,2,3,4,5,6,7,8,9,10)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0,0.25, 0.5,.75, 1)) +
  labs(title = paste("Attractiveness - R score:",coratr), x = "Match Rate", y = "Attractiveness Rating")

test_amb %>% 
  ggplot() +
  aes(x = Decision, y = Ambitious) +
  geom_point(alpha = 1.1) +
  geom_smooth(method = lm,
              se = FALSE, 
              size = 1.5)+
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 1,2,3,4,5,6,7,8,9,10)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0,0.25, 0.5,.75, 1)) +
  labs(title = paste("Ambitiousness - R score:",coramb), x = "Match Rate", y = "Ambitiousness Rating")