#kumar abhinav---author

#setting director and importing datafiles
setwd("C:/Users/Lenovo/Downloads/tds")
train <- read_csv("C:/Users/Lenovo/Downloads/tds/train.csv")
test <- read_csv("C:/Users/Lenovo/Downloads/tds/test.csv")
#examine dataframes
str(train)

#number of people who survived
table(train$Survived)
prop.table(table(train$Survived))

# Create new column in test set with our prediction that everyone dies
test$Survived <- rep(0, 418)
# Create submission dataframe
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
#output file to directory
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
