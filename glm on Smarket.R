#Step1: reading data
install.packages("ISLR", dependencies = T)
library(ISLR)
data("Smarket")
str(Smarket) #note: 'Direction' is our response variable
summary(Smarket)#no NA's
head(Smarket)
tail(Smarket) #1250 records
table(Smarket$Direction)#spits out the weightage of each of the values.
#Since, we'd use this as the response variable ,
#hence we'd convert it into binary, so that we can create a confusionMatrix
Smarket$Direction <- ifelse(Smarket$Direction =='Up', 1, 0)
Smarket$Direction <- as.factor(Smarket$Direction) #casting back as factor
str(Smarket)

#Step2 : Analysing correlation among the variables
Corr_Matrix<- cor(Smarket[,-9]) #as 'Direction' is not a target variable
View(Corr_Matrix)
Corr_Matrix[!lower.tri(Corr_Matrix, diag = F)] <- 0 #spits out just the required cor
View(Corr_Matrix) ##Deduction: Highly uncorrelated variables
plot(Smarket[,-9]) #as 'Direction' is a factor with 2 values.
library(psych)
pairs.panels(Smarket[, -9])
#Deduction: Highly uncorrelated variables


#Step3: Splitting data
library(caret)
index <- createDataPartition(y = Smarket$Direction, times = 1, p = 0.80, list = F)
View(index)
train <- Smarket[index, ]
test <- Smarket[-index, ]

#Step4: training the 'train' dataset
#Step4.1: 10 folds cross validation on stratified data(Approach 2 dataset):
cv.folds <- createMultiFolds(y=train$Direction, k=10, times=5)
cv.cntrl <- trainControl(method = 'repeatedcv',
                         number = 10,
                         repeats = 5,
                         index = cv.folds)
#Step4.2: train
library(doSNOW)
cl <- makeCluster(3, type = 'SOCK')
registerDoSNOW(cl)
glm.cv.1 <- train(Direction ~ .,
                  data = train,
                  method = 'glm',
                  trControl = cv.cntrl,
                  tuneLength = 7)
stopCluster(cl)
glm.cv.1

#Step5: predict
pred <- predict(glm.cv.1, test)
confusionMatrix(pred, test$Direction) #99.6% accuracy
