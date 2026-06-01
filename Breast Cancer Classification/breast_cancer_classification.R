################################################################################
# Breast cancer classification project
################################################################################


# Installation of packages
list_of_packages <- c("readr" # data import
                      ,"dplyr" # data manipulation
                      ,"ggplot2" # data visualization 
                      ,"Information" # Information Value computation
                      ,"gbm" # boosting
                      ,"rpart" # decision trees
                      ,"rpart.plot" # tree's charts
                      ,"randomForest" # random forests
                      ,"ROCR" # model quality assessment - ROC curve, AUC, etc.
                      ,"MASS" # selection of variables for the model
                      ,"adabag" #adaboost
)

# Installing missing libraries
not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)

# Loading libraries
lapply(list_of_packages, library, character = TRUE)



# Loading data
setwd("input_path")
getwd()
breast_cancer <- read.csv('breast-cancer-wisconsin.data')
breast_cancer <- data.frame(lapply(breast_cancer, as.integer))
colnames(breast_cancer) <- c('sample_nr', 'clump_thic', 'unif_cell_size', 'unif_cell_shape', 'marg_adhesion', 
                             'cell_size', 'bare_nuclei', 'chromatin', 'normal_nucleoli', 'mitoses', 'class_col')
breast_cancer$class_col <- ifelse(breast_cancer$class_col > 3, 1, 0)

# Description of the dataset
# 1. Sample code number: Unique identifier for each tissue sample.
# 2. Cluster thickness: Assessment of the thickness of tumour cell clusters (1 - 10).
# 3. Cell size uniformity: Uniformity of tumour cell size (1 - 10).
# 4. Uniformity of cell shape: Uniformity of tumour cell shape (1 - 10).
# 5. Marginal adhesion: Degree of adhesion of tumour cells to surrounding tissue (1 - 10).
# 6. Single epithelial cell size: Size of individual tumour cells (1 - 10).
# 7. Exposed nuclei: Presence of nuclei without surrounding cytoplasm (1 - 10).
# 8. Bland Chromatin: Assessment of chromatin structure in tumour cells (1 - 10).
# 9. Normal nuclei: Presence of normal-looking nuclei in tumour cells (1 - 10).
# 10 Mitoses: Frequency of mitotic cell divisions (1 - 10).
# 11. Class: Classification of tumour type (0 for benign, 1 for malignant).


# Initial data analysis
str(breast_cancer)
summary(breast_cancer)

# Check for NA and NULL values in dataframe
sa_na <- any(is.na(breast_cancer))
sa_null <- any(sapply(breast_cancer, function(x) all(is.null(x))))
print(paste("Czy są wartości NA:", any(is.na(breast_cancer))))
print(paste("Czy są wartości NULL:", sa_null))

# All NA are in the bare_nuclei column
sum(is.na(breast_cancer)) == sum(is.na(breast_cancer$bare_nuclei))
# delete records with NA values
breast_cancer <- na.omit(breast_cancer)


# Plots for each variable 
column_names <- colnames(breast_cancer)[!(colnames(breast_cancer) %in% c("class_col", "sample_nr"))]

for (col in column_names) {
  # Calculate the percentage of positive class for each bin
  data_summary <- breast_cancer %>%
    group_by_at(col) %>%
    summarise(total = n(),
              positive = sum(class_col == 1)) %>%
    mutate(positive_percentage = positive / total * 100)
  
  total_count <- sum(data_summary$total)
  
  # ggplot 
  variable_plot <- ggplot(data_summary, aes_string(x = col, y = "total", fill = "positive_percentage")) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "darkblue", high = "deepskyblue", name = "% klasy pozytywnej") +
    scale_y_continuous(
      name = "Liczba obserwacji",
      sec.axis = sec_axis(~ . / total_count, name = "Udział procentowy", labels = scales::percent)
    ) +
    labs(title = paste("Histogram zmiennej", col, "z udziałem klasy pozytywnej"),
         x = col) +
    theme_bw()
  
  print(variable_plot)
}


## Information Value
infoTables <- create_infotables(data = breast_cancer,
                                y = 'class_col',
                                bins = 2,
                                parallel = F)

# IV plot
IV_table <- infoTables$Summary[order(-infoTables$Summary$IV), ]
IV_table$Variable <- factor(IV_table$Variable,
                            levels = IV_table$Variable[order(-IV_table$IV)])

ggplot(IV_table, aes(x = Variable, y = IV)) +
  geom_bar(witdh = .35, stat = 'identity', color = 'darkblue', fill = 'darkblue') +
  ggtitle('Information value') +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))



# Selecting variables and setting the class variable as a factor
breast_cancer <- breast_cancer %>%
  dplyr::select(-c(sample_nr)) %>%
  mutate(class_col = as.factor(class_col))


# Structure of the class variable in the dataset
class_structure <- breast_cancer %>%
  count(class_col) %>%
  mutate(percent = round(( 100 * n / nrow(breast_cancer)), 2)) %>%
  print()

ggplot(class_structure, aes(x = class_col, y = n, fill = class_col)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(n, " (", percent, "%)", sep = "")), vjust = -0.5) +
  labs(title = "Struktura klas w zbiorze danych",
       x = "Klasa",
       y = "Liczba obserwacji",
       fill = "Klasa") +
  theme_classic()



# Division of the dataset into training and test dataset
set.seed(420)
test_prop <- 0.25 
test.set.index <- (runif(nrow(breast_cancer)) < test_prop)
breast_cancer_test <- breast_cancer[test.set.index, ]
breast_cancer_train <- breast_cancer[!test.set.index, ]


################################################################################
# Models
################################################################################

## Logistic regression
reg_log_full <- glm(class_col ~ ., 
                    data = breast_cancer_train, 
                    family = binomial)

summary(reg_log_full)

## Logistic regression, variable selection using Stepwise algorithm  [Akaike information criterion (AIC)]
reg_log_step <- reg_log_full %>%
  stepAIC(trace = FALSE)

coef(reg_log_full)
coef(reg_log_step)

summary(reg_log_step)

# To interpret the result, the coefficients (expressed in logarithms) are multiplied to obtain the odds ratio
exp(coef(reg_log_full))
exp(coef(reg_log_step))


## Decision tree v1
tree_1 <- rpart(class_col ~ .,
              data = breast_cancer_train,
              method = "class")

rpart.plot(tree_1, under = FALSE, tweak = 1.3, fallen.leaves = TRUE, shadow.col = "gray")
rpart.plot(tree_1, type = 4, under = TRUE, tweak = 1.2, fallen.leaves = TRUE)


## Decision tree v2 (stump)
tree_2 <- rpart(class_col ~ .,
                data = breast_cancer_train,
                method = "class",
                cp = 0.06)

rpart.plot(tree_2, under = FALSE, tweak = 1.3, fallen.leaves = TRUE, shadow.col = "gray")

# Significance of variables
tree_1$variable.importance
tree_2$variable.importance

## Bagging
# We use randomForest, set mtry = number of explanatory variables
bagging <- randomForest(class_col ~., data = breast_cancer_train, mtry = 9, ntree = 500, importance = TRUE)

# Significance of variables
varImpPlot(bagging)
bagging$importance

## Random forest
rf_1 <- randomForest(class_col ~., data = breast_cancer_train, ntree = 500, mtry = sqrt(9))

# Significance of variables
varImpPlot(rf_1)
rf_1$importance

## Boosting (AdaBoost)
boosting <- boosting(class_col~., data=breast_cancer_train, boos=TRUE, mfinal=100)

summary(boosting)
print(boosting)



################################################################################
# Evaluation of models
################################################################################

## Confusion matrix
  # list of all models
modele <- list(reg_log_full, reg_log_step, tree_1, tree_2, bagging, rf_1, boosting) 
  # list of all models names
names(modele) <- c("reg_log_full", "reg_log_step", "tree_1", "tree_2", "bagging", "rf_1", "boosting") 




# Function to evaluate the model
EvaluateModel <- function(predictions, actual) {
  confusion_matrix <- table(predictions, actual)
  
  true_positive <- confusion_matrix[2, 2]
  true_negative <- confusion_matrix[1, 1]
  false_positive <- confusion_matrix[2, 1]
  false_negative <- confusion_matrix[1, 2]
  
  condition_positive <- sum(confusion_matrix[, 2])
  condition_negative <- sum(confusion_matrix[, 1])
  
  predicted_positive <- sum(confusion_matrix[2, ])
  predicted_negative <- sum(confusion_matrix[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(confusion_matrix)
  MER <- 1 - accuracy # Misclassification Error Rate
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  
  list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy, 
    MER = MER,
    precision = precision,
    sensitivity = sensitivity,
    specificity = specificity,
    F1 = F1
  )
}

# Initialize lists to store evaluation results and probability predictions
evaluation_results <- list()
preds <- list()

# Iterate through the list of models
for (model_name in names(modele)) {
  model <- modele[[model_name]]
  
  # Generate predictions and probabilities
  if (model_name %in% c("reg_log_full", "reg_log_step")) {
    predictions <- ifelse(predict(model, newdata = breast_cancer_test, type = "response") > 0.5, 1, 0)
    probability_predictions <- as.vector(predict(model, newdata = breast_cancer_test, type = "response"))
  } else if (model_name %in% c("tree_1", "tree_2")) {
    predictions <- predict(model, newdata = breast_cancer_test, type = "class")
    probability_predictions <-  as.vector(predict(model, newdata = breast_cancer_test)[, 2])
  } else if (model_name == "boosting") {
    step <- predict(boosting, newdata = breast_cancer_test)
    predictions <- step$class
  } else {
    predictions <- predict(model, newdata = breast_cancer_test, type = "class")
    probability_predictions <- as.vector(predict(model, newdata = breast_cancer_test, type = "prob")[, 2])
  }
  
  # Evaluate the model
  evaluation_result <- EvaluateModel(predictions, breast_cancer_test$class_col)
  evaluation_results[[paste0(model_name, "_EvaluateModel")]] <- evaluation_result
  
  # Store the probability predictions
  preds[[model_name]] <- probability_predictions
  
  # Print the evaluation results
  cat("Evaluation results for model", model_name, ":\n")
  print(evaluation_result$confusion_matrix)
  cat("Accuracy:", evaluation_result$accuracy, "\n")
  cat("Misclassification Error Rate (MER):", evaluation_result$MER, "\n")
  cat("Precision:", evaluation_result$precision, "\n")
  cat("Sensitivity (Recall / TPR):", evaluation_result$sensitivity, "\n")
  cat("Specificity:", evaluation_result$specificity, "\n")
  cat("F1 Score:", evaluation_result$F1, "\n\n")
}

## ROC curve (Receiver Operating Characteristic) 
plot(performance(prediction(preds[["reg_log_full"]], breast_cancer_test$class_col), "tpr", "fpr"), lwd = 2, colorize = TRUE) 

for (i in 1:length(preds)) {
  plot(performance(prediction(preds[[i]], breast_cancer_test$class_col), "tpr", "fpr"), lwd = 2, colorize = FALSE, col = i, add = ifelse(i == 1, FALSE, TRUE)) 
}

abline(coef = c(0, 1), lty = 2, lwd = 0.5)

legend("bottomright", 
       legend = names(preds),
       col = 1:length(preds), 
       lty = rep(1, length(preds))
)

## AUC (Area Under Curve) - under ROC curve
auc_list <- list()
for (i in 1:length(preds)) {
  auc <- performance(prediction(preds[[i]], breast_cancer_test$class_col), "auc")@y.values[[1]]
  cat(names(preds)[i], ": ", auc, "\n")
  auc_list[[names(preds)[i]]] <- auc
}

# Sorting the AUC values in descending order
auc_df <- data.frame(Model = names(auc_list), AUC = unlist(auc_list))
auc_df <- auc_df[order(-auc_df$AUC), ]

for (i in 1:nrow(auc_df)) {
  cat(auc_df$Model[i], ": ", auc_df$AUC[i], "\n")
}




## Lift chart
plot(performance(prediction(preds[["reg_log_full"]], breast_cancer_test$class_col), "lift", "rpp"), lwd = 2, col = "darkblue") 

for (i in 1:length(preds)) {
  plot(performance(prediction(preds[[i]], breast_cancer_test$class_col), "lift", "rpp"), lwd = 2, colorize = FALSE, col = i, lty = i, add = ifelse(i == 1, FALSE, TRUE)) 
}

legend("bottomleft", 
       legend = names(preds),
       col = 1:length(preds), 
       lty = 1:length(preds)
)