### The large bulk of the work has been done in the previous files. Now, it 
### remains to apply an appropiate models to predict the binary classes
### of the validation set as well as possible.

### Random Forest
library(randomForest)

set.seed(1) # set a seed for reproducibility
rf <- randomForest(Class ~ ., data = imputed.train1, na.action = na.omit, 
                   ntree = 350)

# Prediction
predict.rf <- predict(rf, imputed.val1, type="prob")

library(pROC)
result.roc.rf <-  roc(val$Class, predict.rf[, 1], auc=TRUE)
auc(result.roc.rf)
plot(result.roc.rf, print.thres="best", print.thres.best.method="closest.topleft")