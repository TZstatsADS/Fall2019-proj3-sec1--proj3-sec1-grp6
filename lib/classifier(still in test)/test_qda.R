########## QDA Classifier ##########
library(MASS)

test <- function(train_dt = dat_train, test_dt = dat_test){
  
  ### Input: 
  ###  - the fitted classification model using training data
  ### (since knn does not need to train, we only specify k here)
  ###  - processed features from testing images 
  ### Output: training model specification
  
  train_data <- train_dt[,-which(names(train_dt) == 'emotion_idx')] %>% scale
  test_data <- test_dt[,-which(names(test_dt) == 'emotion_idx')] %>% scale
  
  feature_chosen <- sample(1:6006,45,replace=F)
  
  train_data <- train_data[,feature_chosen]
  test_data <- test_data[,feature_chosen]
  ### Model with QDA Classifier
  
  class <- train_dt$emotion_idx
  qda_model <- qda(train_data,class)
  
  ### Making Predictions
  model_prd <- predict(qda_model,test_data)$class
  
  return(model_prd)
}

# pred <- test()
# mean(pred==dat_test$emotion_idx)