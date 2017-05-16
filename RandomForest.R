#Load libraries 
library(readr)
library(randomForest)
library(zoo)

#Read all datasets from csv files from working directory
Base1 <- read_csv("./Base1.csv")
Base2 <- read_csv("./Base2.csv")
test_csv <- read_csv("./test.csv")
train_csv <- read_csv("./train.csv")

# print first 6 rows of datasets    
head(Base1)
head(Base2)
head(test_csv)
head(train_csv)

# summary of  datasets  
summary(Base1)
summary(Base2)
summary(test_csv)
summary(train_csv)

# str of  datasets
str(Base1)
str(Base2)
str(test_csv)
str(train_csv)

#check missing values
table(is.na(Base2))

#returns the percentage of missing values per column
sapply(Base2, function(x) sum(is.na(x))/length(x))*100

# use library zoo for replace NA by median
Base2<- na.aggregate(Base2, FUN = median)
summary(Base2)

#Change character as.factor for categorical variables
test_csv$Target <- as.factor(test_csv$Target)
train_csv$Target <- as.factor(train_csv$Target)
Base1$T2 <- as.factor(Base1$T2)
Base1$T3 <- as.factor(Base1$T3)
Base1$T4 <- as.factor(Base1$T4)
Base1$T5 <- as.factor(Base1$T5)

# delete T2&T3, columns contain more than 53 factor levels (not applicable for Random Forest)
Base1 <- subset(Base1, select = -c(T2,T3)) 

# Create dataset test . Merge test_cvs + Base1 and Base2 for by column ID
testB1<- merge(test_csv,Base1,by="ID")
test<- merge(testB1,Base2,by="ID")

# Create dataset train . Merge test_cvs + Base1 and Base2 for by column ID
trainB1<- merge(train_csv,Base1,by="ID")
train<- merge(trainB1,Base2,by="ID")

#Select  data only for the last month 
trainM1 <- subset (train, train$MONTH_AGO == 6 )
testM1 <- subset (test, test$MONTH_AGO == 6 )

# Simple Random Forest
rf<-randomForest(Target~., data=trainM1, ntree = 400,mtry = 6,importance = TRUE,
                   proximity = TRUE, na.action=na.omit)
print(rf)

# Variable Importance plot with  TOP 15 
varImpPlot(rf,
           sort = T,
           n.var = 15,
           main = "Top 15 - Variable Importance")

p <- predict(rf, testM1)
table(p)

# instert prediction data to test data
test_csv$Target<-p
write.csv(test_csv, file = "test.csv",row.names=F)
