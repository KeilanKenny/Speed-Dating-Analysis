# ----------------------------------------
# Machine Learning Model
# ----------------------------------------
# This script models the data:
# 1. First Model
# 2. Second Model
# 3. Plot ROC Curves
# ----------------------------------------

# Load necessary libraries
library(ggplot2)
library(dplyr)
require(forcats)
library(ROCR)

# ----------------------------------------
# First Model
# ----------------------------------------

# Create a subset of the data for modeling
model_data2 <- subset(
  data,
  select = c(
    'samerace', 'dec_o', 'race', 'goal', 'date', 'go_out', 'career_c', 'dec',
    'attr', 'sinc', 'intel', 'fun', 'amb', 'shar', 'sports', 'tvsports',
    'exercise', 'dining', 'museums', 'art', 'hiking', 'gaming', 'clubbing',
    'reading', 'tv', 'theater', 'movies', 'concerts', 'music', 'shopping', 'yoga'
  )
)

# Replace NA values with column mean
for (i in 1:ncol(model_data2)) {
  model_data2[is.na(model_data2[, i]), i] <- mean(model_data2[, i], na.rm = TRUE)
}

# Remove any columns with NA values
model_data2 <- model_data2 %>%
  select_if(~ !any(is.na(.)))

# Split the data into training and test sets (70% training, 30% testing)
sample <- sample(c(TRUE, FALSE), nrow(model_data2), replace = TRUE, prob = c(0.7, 0.3))
train2 <- model_data2[sample, ]
test2  <- model_data2[!sample, ]

# Fit logistic regression model
model2 <- glm(dec ~ ., family = binomial(link = 'logit'), data = train2)

# Show summary of the model
summary(model2)

# Perform Chi-squared test on the model
anova(model2, test = "Chisq")

# Make predictions on the test data
fitted.results <- predict(model2, test2, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

# Calculate misclassification error and accuracy
misClasificError <- mean(fitted.results != test2$dec)
accuracy <- 1 - misClasificError
accuracy2 <- format(round(accuracy, 2), nsmall = 2)
print(paste('Accuracy:', accuracy2))

# Create and evaluate the ROC curve
p <- predict(model2, newdata = subset(test2), type = "response")
pr <- prediction(p, test2$dec)
prf2 <- performance(pr, measure = "tpr", x.measure = "fpr")

# Calculate and format AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc2 <- format(round(auc, 2), nsmall = 2)

# Plot ROC curve and display accuracy and AUC on the title
plot(prf2,
     main = 'ROC Curve Model 1',
     xlab = "False Positive Rate",
     ylab = "True Positive Rate")
title(paste("Accuracy:", accuracy2, "| AUC:", auc2), line = 0.35, font.main = 1)

# ----------------------------------------
# Second Model
# ----------------------------------------

# Create a subset of the data for modeling
model_data3 <- subset(
  data,
  select = c(
    'gender', 'dec_o', 'race', 'dec', 'attr', 'fun', 'amb', 'shar', 'sports',
    'clubbing', 'yoga', 'sinc', 'date', 'movies', 'shopping', 'music', 'reading',
    'gaming', 'museums', 'career_c', 'date', 'samerace'
  )
)

# Replace NA values with column mean
for (i in 1:ncol(model_data3)) {
  model_data3[is.na(model_data3[, i]), i] <- mean(model_data3[, i], na.rm = TRUE)
}

# Remove any columns with NA values
model_data3 <- model_data3 %>%
  select_if(~ !any(is.na(.)))

# Split the data into training and test sets (70% training, 30% testing)
sample <- sample(c(TRUE, FALSE), nrow(model_data3), replace = TRUE, prob = c(0.7, 0.3))
train3 <- model_data3[sample, ]
test3  <- model_data3[!sample, ]

# Fit logistic regression model
model3 <- glm(dec ~ ., family = binomial(link = 'logit'), data = train3)

# Show summary of the model
summary(model3)

# Perform Chi-squared test on the model
anova(model3, test = "Chisq")

# Make predictions on the test data
fitted.results <- predict(model3, test3, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

# Calculate misclassification error and accuracy
misClasificError <- mean(fitted.results != test3$dec)
accuracy <- 1 - misClasificError
accuracy3 <- format(round(accuracy, 2), nsmall = 2)
print(paste('Accuracy:', accuracy3))

# Create and evaluate the ROC curve
p <- predict(model3, newdata = subset(test3), type = "response")
pr <- prediction(p, test3$dec)
prf3 <- performance(pr, measure = "tpr", x.measure = "fpr")

# Calculate and format AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc3 <- format(round(auc, 2), nsmall = 2)

# Plot ROC curve and display accuracy and AUC on the title
plot(
  prf3,
  main = 'ROC Curve Model 2',
  xlab = "False Positive Rate",
  ylab = "True Positive Rate"
)
title(paste("Accuracy:", accuracy3, "| AUC:", auc3), line = 0.35, font.main = 1)

# ----------------------------------------
# Plot both ROC Curves
# ----------------------------------------

# Plot the first ROC curve (Model 1)
plot(
  prf2,
  main = 'ROC Curve',
  col = "red",
  lwd = 2
)

# Plot the second ROC curve (Model 2) on top of the first
plot(
  prf3,
  col = "blue",
  lwd = 2,
  add = TRUE
)

# Add a legend to the bottom right of the plot
legend(
  "bottomright",
  legend = c(
    paste("Model 1 | Accuracy:", acc2, "& AUC:", auc2),
    paste("Model 2 | Accuracy:", acc3, "& AUC:", auc3)
  ),
  col = c("red", "blue"),
  lwd = 2
)