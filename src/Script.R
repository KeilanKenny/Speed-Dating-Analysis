# Packages
library(ggplot2)
library(dplyr)
require(forcats)
library(ROCR)

# Read in data
data <- read.csv('Speed Dating Data.csv')

# Information about the data
str(data)
summary(data)

# Number of NA values per column.
colSums(is.na(data))





# Particpants demographics
## Age 
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
  
  
## Race
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
  
  
  
## Career
data_career <- na.omit(data %>% group_by(iid, career_c, gender) %>% summarise(.groups = 'drop'))
data_career$gender <- with(data_career, ifelse(data_career$gender == 0, 'Female', 'Male'))


unique(data_career$career_c)
data_career$career_c <- with(data_career, ifelse(data_career$career_c == 1, 'Lawyer', 
                                        ifelse(data_career$career_c == 2, 'Academic/Research',
                                        ifelse(data_career$career_c == 3, 'Psychologist',
                                        ifelse(data_career$career_c == 4, 'Doctor/Medicine', 
                                        ifelse(data_career$career_c == 5, 'Engineer',
                                        ifelse(data_career$career_c == 6, 'Creative Arts/Entertainment',
                                        ifelse(data_career$career_c == 7, 'Banking/Consulting/Finance/Marketing/Business/CEO/Entrepreneur/Admin',
                                        ifelse(data_career$career_c == 8, 'Real Estate',
                                        ifelse(data_career$career_c == 9, 'International/Humanitarian Affairs',
                                        ifelse(data_career$career_c == 10, 'Undecided',
                                        ifelse(data_career$career_c == 11, 'Social Work',
                                        ifelse(data_career$career_c == 12, 'Speech Pathology',
                                        ifelse(data_career$career_c == 13, 'Politics',
                                        ifelse(data_career$career_c == 14, 'Pro sports/Athletics',
                                        ifelse(data_career$career_c == 15, 'Other',
                                        ifelse(data_career$career_c == 16, 'Journalism', 'Architecture')))))))))))))))))

unique(data_career$career_c)



ggplot(data_career) +
  geom_bar(position="dodge",aes(career_c, fill=gender), color = "black") + 
  scale_fill_discrete("Gender", labels = c("Female", "Male")) +
  labs(title = "Career Distribution", x = "Career", y = "Count") +
  coord_flip()



# Participants choices
## goals
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









# Correlation

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




#  Modelling

## first model
model_data2 <- subset(data, select=c('samerace','dec_o','race','goal','date','go_out','career_c','dec','attr','sinc','intel','fun','amb','shar','sports','tvsports','exercise','dining','museums','art','hiking','gaming','clubbing','reading', 'tv','theater', 'movies', 'concerts','music','shopping','yoga'))


for(i in 1:ncol(model_data2)){
  model_data2[is.na(model_data2[,i]), i] <- mean(model_data2[,i], na.rm = TRUE)
}
model_data2 <- model_data2 %>% select_if(~ !any(is.na(.)))

sample <- sample(c(TRUE, FALSE), nrow(model_data2), replace=TRUE, prob=c(0.7,0.3))
train2  <- model_data2[sample, ]
test2  <- model_data2[!sample, ]


model2 <- glm(dec ~.,family=binomial(link='logit'),data=train2)
summary(model2)

anova(model2, test="Chisq")

fitted.results <- predict(model2, test2,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test2$dec)
print(paste('Accuracy',1-misClasificError))
acc<-1-misClasificError
acc2 <- format(round(acc, 2), nsmall = 2)
acc2

p <- predict(model2, newdata=subset(test2), type="response")
pr <- prediction(p, test2$dec)
prf2 <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc2 <- format(round(auc, 2), nsmall = 2)
auc2

plot(prf2, 
     main = 'ROC Curve Model 1',
     xlab="False Postive Rate",
     ylab="True Postive Rate",)
title(paste("Accuracy:",acc2,"| AUC:",auc2),line=0.35,font.main = 1)



## Second model
model_data3 <- subset(data, select=c('gender','dec_o','race','dec','attr','fun','amb','shar','sports','clubbing','yoga',    'sinc','date','movies','shopping','music','reading','gaming','museums','career_c','date','samerace'))


for(i in 1:ncol(model_data3)){
  model_data3[is.na(model_data3[,i]), i] <- mean(model_data3[,i], na.rm = TRUE)
}
model_data3 <- model_data3 %>% select_if(~ !any(is.na(.)))

sample <- sample(c(TRUE, FALSE), nrow(model_data3), replace=TRUE, prob=c(0.7,0.3))
train3  <- model_data3[sample, ]
test3  <- model_data3[!sample, ]


model3 <- glm(dec ~.,family=binomial(link='logit'),data=train3)
summary(model3)

anova(model3, test="Chisq")

fitted.results <- predict(model3, test3,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test3$dec)
print(paste('Accuracy',1-misClasificError))
acc<-1-misClasificError
acc3 <- format(round(acc, 2), nsmall = 2)
acc3

p <- predict(model3, newdata=subset(test3), type="response")
pr <- prediction(p, test3$dec)
prf3 <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc3 <- format(round(auc, 2), nsmall = 2)
auc3

plot(prf3, 
     main = 'ROC Curve Model 2',
     xlab="False Postive Rate",
     ylab="True Postive Rate",)
title(paste("Accuracy:",acc3,"| AUC:",auc3),line=0.35,font.main = 1)




## Plot both ROC Curves
plot(prf2,main = 'ROC Curve', col = "red", lwd = 2)

plot(prf3, col = "blue", lwd = 2, add = TRUE)

legend("bottomright", legend = c(paste(" Model 1 | Accuracy:",acc2,"& AUC:",auc2), paste(" Model 2 | Accuracy:",acc3,"& AUC:",auc3)), col = c("red", "blue"), lwd = 2)

