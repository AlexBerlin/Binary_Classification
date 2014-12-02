# read in the data (located in WD)
train <- read.csv2("training.csv")

val <- read.csv2("validation.csv")

###
### Some basic non-graphical EDA
###
str(train)
str(val)

summary(train)
summary(val)

# look at classes of the two data sets
sapply(train, class)
sapply(val, class)

# Compare if all classes are identical
all(sapply(train, class) == sapply(val, class))

# give new name to the response variable to avoid confusion later
names(train)[19] <- "Class"
names(val)[19] <- "Class"

# look at missmap
library(Amelia)
missmap(train)
missmap(val)

# look at (amount of) missing values
apply(train, 2, function(x) length(which(is.na(x))))
apply(val, 2, function(x) length(which(is.na(x))))