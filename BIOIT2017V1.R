# Logistic Regression Model

# Importing the dataset
OVdata = read.delim('BioIT2017_data254OV_SvR_v1.tsv')

OVdata = OVdata[2:52]
names(OVdata) = sub(" ", ".", names(OVdata))

# Encoding categorical data, encoding the target feature as a factor
OVdata$Platinum.Status = factor(OVdata$Platinum.Status, levels = c('Sensitive', 'Resistant'), labels = c(0, 1))

# Splitting the dataset into the Training set and Test set

library(caret)

# Scale features

normOVvalues <- preProcess(OVdata, method = c("center", "scale"))
normOVdata <- predict(normOVvalues, OVdata)

set.seed(123)

# Split into 75% training and 25% testing sets

trainIndex <- createDataPartition(normOVdata$Platinum.Status, p = .75, list = FALSE, times = 1)

OVtrain <- normOVdata[ trainIndex,]
OVtest  <- normOVdata[-trainIndex,]

# Initialize accuracy vector

acc <- rep(0, times = 51)
acc[1] = 0.50
acc[2] = 0.65

for (nFeat in 3:49){
  
  print(paste("# Features: ", nFeat))
  
  thisOVtrain = OVtrain[, 1:(1+nFeat)]
  
  # Fitting Logistic Regression to the Training set
  
  logReg = glm(formula = Platinum.Status ~ ., family = binomial, data = thisOVtrain)
  
  # Predict the Test set results
  
  p_pred = predict(logReg, type = 'response', newdata = OVtest[, 1:(1+nFeat)])
  y_pred = ifelse(p_pred > 0.5, 1, 0)
  
  # Make Confusion Matrix
  
  confMat = table(OVtest[, 1], y_pred > 0.5)
  
  acc[nFeat] = (confMat[1,1] + confMat[2,2]) / (confMat[1,1] + confMat[1,2] + confMat[2,1] + confMat[2,2])
  
}

plot(acc, col="orange", lwd=3, main="Model Complexity", ylim=c(0.5,1), xlab="# Features", ylab="Accuracy")

#fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

nFeat = 10

thisOVtrain = OVtrain[, 1:(1+nFeat)]
logReg = glm(formula = Platinum.Status ~ ., family = binomial, data = thisOVtrain)
p_pred = predict(logReg, type = 'response', newdata = OVtest[, 1:(1+nFeat)])
y_pred = ifelse(p_pred > 0.5, 1, 0)
confMat = table(OVtest[, 1], y_pred > 0.5)

# Plot ROC curve

library(pROC)

plot(roc(OVtest$Platinum.Status, p_pred, direction="<"), col="orange", lwd=3, main="Platinum Resistance Prediction")
