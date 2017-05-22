# Logistic Regression Model

# Importing the dataset
dataset = read.delim('/home/chdemo/BIO-ITdemo/BioIT2017_data254OV_SvR_v1.tsv')

dataset = dataset[2:52]
names(dataset) = sub(" ", ".", names(dataset))

# Encoding categorical data, encoding the target feature as a factor
dataset$Platinum.Status = factor(dataset$Platinum.Status, levels = c('Resistant', 'Sensitive'), labels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')

library(caTools)
set.seed(123)
split = sample.split(dataset$Platinum.Status, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[, 2:51] = scale(training_set[, 2:51])
test_set[, 2:51] = scale(test_set[, 2:51])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Platinum.Status ~ ., family = binomial, data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[, 2:51])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred > 0.5)

