library(ggplot2)

### Perform Exploratory Data Analysis to better understand the data and
### perform imputation and model selection later.

### Response variable
ggplot(train, aes(x=Class)) + geom_bar() # very skewed class
ggplot(val, aes(x=Class)) + geom_bar()

# The response variable is skewed in the training set and roughly 50/50 in the
# validation set. This should be kept in mind for the classification later.

###
### Categorical values
###

#v1
ftable(factor(train$v1, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v1)) + geom_bar(aes(fill = Class)) 
# There are only a few NAs here and there is not an obvious way to deal with
# these NAs.

ftable(factor(val$v1, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v1)) + geom_bar(aes(fill = Class)) # only a few NAs

#v5
ftable(factor(train$v5, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v5)) + geom_bar(aes(fill = Class))

# Here, we see that the NA values are all "yes". Therefore, I will assign the NA
# values to the "gg" level which is also entirely "yes".
train[is.na(train$v5) == TRUE, "v5"] <- "gg"

ftable(factor(val$v5, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v5)) + geom_bar(aes(fill = Class)) # only a few NAs

#v6
ftable(factor(train$v6, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v6)) + geom_bar(aes(fill = Class))

# The ratio of the NA values in the yes/no groups is the same as the "r" levels.
# Therefore, I will assign the NA values to this level.

train[is.na(train$v6) == TRUE, "v6"] <- "r"

ftable(factor(val$v6, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v6)) + geom_bar(aes(fill = Class)) # only a few NAs


#v7
ftable(factor(train$v7, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v7)) + geom_bar(aes(fill = Class))

# Same as "v6", but with the "n" level.
train[is.na(train$v7) == TRUE, "v7"] <- "n"

ftable(factor(val$v7, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v7)) + geom_bar(aes(fill = Class)) # only a few NAs

#v9
ftable(factor(train$v9, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v9)) + geom_bar(aes(fill = Class))

ftable(factor(val$v9, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v9)) + geom_bar(aes(fill = Class)) # only a few NAs

#v10
ftable(factor(train$v10, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v10)) + geom_bar(aes(fill = Class))

ftable(factor(val$v10, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v10)) + geom_bar(aes(fill = Class)) # only a few NAs

#v12
ftable(factor(train$v12, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v12)) + geom_bar(aes(fill = Class))

ftable(factor(val$v12, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v12)) + geom_bar(aes(fill = Class)) # only a few NAs

#v13
ftable(factor(train$v13, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v13)) + geom_bar(aes(fill = Class))

ftable(factor(val$v13, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v13)) + geom_bar(aes(fill = Class)) # only a few NAs

#v18
ftable(factor(train$v18, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v18)) + geom_bar(aes(fill = Class))
# Lots of NAs here...

ftable(factor(val$v18, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v18)) + geom_bar(aes(fill = Class))

# The ratio of the NAs in v18 in the training and validation set is quite 
# similar to the ratio of the response variable (yes/no) in the respective set. 
# Therefore, I will create a new level for the this predictor which replaces the
# NAs values.

train$v18 <- as.character(train$v18)
val$v18 <- as.character(val$v18)

train$v18[is.na(train$v18)] <- "M"
val$v18[is.na(val$v18)] <- "M"

train$v18 <- factor(train$v18)
val$v18 <- factor(val$v18)

#v19
ftable(factor(train$v19, exclude=NULL) ~ train$Class, useNA="always")
ggplot(train, aes(v19)) + geom_bar(aes(fill = Class))
# v19 is a perfect predictor for the response variable. Therefore it should be
# removed.
train$v19 <- NULL

ftable(factor(val$v19, exclude=NULL) ~ val$Class, useNA="always")
ggplot(val, aes(v19)) + geom_bar(aes(fill = Class))

head(val$v19, 30)
all(rep(c(1,0), length.out = length(val$v19)) == val$v19)
# so this predictor just repeats 1,0 for its entire length and is therefore
# useless for the prediction.
val$v19 <- NULL

## look at missing values for the categorical values again
nums <- sapply(train, is.numeric)
nums.val <- sapply(val, is.numeric)

apply(train[, -nums], 2, function(x) length(which(is.na(x))))
apply(val[, -nums.val], 2, function(x) length(which(is.na(x))))

# look at missmap
missmap(train[, -nums])
missmap(val[, -nums.val])

dim(train[!complete.cases(train[, -nums]), ])
dim(val[!complete.cases(val[, -nums.val]), ])
# Only few NAs remain for both sets


###
### Numeric values
###

# Let's look at some correlation chart
library(PerformanceAnalytics)

chart.Correlation(train[, nums])
# It's easy to see that v14 and v17 have a perfect correlation. We should remove
# one of the predictors to avoid issues of collinearity.
train$v14 <- NULL

chart.Correlation(val[, nums.val]) 
# same issue for the validation set
val$v14 <- NULL

# How many NAs remain?
apply(train[, nums], 2, function(x) length(which(is.na(x))))
apply(val[, nums.val], 2, function(x) length(which(is.na(x))))

# This step needs to be repeated, as the DFs changed
nums <- sapply(train, is.numeric)
nums.val <- sapply(val, is.numeric)

### Normalize numeric part of the data

train[, nums] <- scale(train[, nums])

### Use Amelia to impute the missing numerical values

factor.columns <- c("v1", "v4", "v5", "v6", "v7", "v9", "v10", "v12", "v13", 
                    "v18")

# Here, I don't use the response variable for imputation...
amelia.train <- amelia(x = subset(train, select=-c(Class)), idvars = factor.columns)

summary(amelia.train)

# save the result as a data frame
imputed.train1 <- amelia.train$imputations[[1]]

## I will do the same for the validation set
## Again, I won't use the response variable

amelia.val <- amelia(x = subset(val, select=-c(Class)), idvars = factor.columns)

summary(amelia.val)

imputed.val1 <- amelia.val$imputations[[1]]

## look at missing values for the categorical values again
apply(imputed.train1[, nums], 2, function(x) length(which(is.na(x))))
apply(imputed.val1[, nums.val], 2, function(x) length(which(is.na(x))))
# No NAs remain...

### Final look at missing values
dim(imputed.train1[!complete.cases(imputed.train1), ]) # 103 values
dim(imputed.val1[!complete.cases(imputed.val1), ]) # 5 values

# Add response variable to the final sets again
imputed.train1$Class <- train[, "Class"]
imputed.val1$Class <- val[, "Class"]

# look at the different levels of the data sets
for (i in factor.columns) {
  print(paste("Feature name is", i))
  print(setdiff(levels(imputed.train1[, i]), levels(imputed.val1[, i])))
  print(setdiff(levels(imputed.val1[, i]), levels(imputed.train1[, i])))
}
# There should be a function for this...

# Fix the differences
levels(imputed.val1$v4) <- levels(imputed.train1$v4)
levels(imputed.val1$v5) <- levels(imputed.train1$v5)
levels(imputed.val1$v6) <- levels(imputed.train1$v6)
levels(imputed.val1$v7) <- levels(imputed.train1$v7)
