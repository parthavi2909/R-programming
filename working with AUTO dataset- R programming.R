install.packages('ISLR')
library(ISLR)
dim(Auto)
Auto[1:4,]
Auto = na.omit(Auto)
names(Auto)
df = Auto
plot(df$cylinder,df$mpg)
attach(df)
cylinders = as.factor(cylinders)
plot(cylinders,mpg)
plot(cylinders,mpg,col="green",varwidth = T,xlab = "cylinders",ylab = "MPG")
plot(cylinders,mpg,col="green",varwidth = T)

hist(mpg,col = 3,breaks = 15)
pairs(t)
pairs(~mpg+displacement+horsepower+weight+acceleration,df)
plot(horsepower,mpg)
summary(df)
head(df)
library(caret)


validation_index <- createDataPartition(horsepower, p=0.80, list=FALSE)
validation <- df[-validation_index,]
df<- df[validation_index,]
control <- trainControl(method="cv", number=10)
metric <- "none"


#Linear Regression
set.seed(7)
fit.lm <- train(horsepower~., data=df, method="lm", metric=metric, trControl=control)
print(fit.lm)
predictions <- predict(fit.lm, validation)
x = R2(validation$horsepower, predictions, form = "traditional")
print(x)

#SVM regression
library(e1071)
model_reg = svm(horsepower~., data=df)
print(model_reg)
pred = predict(model_reg, validation)
print(pred)
y = R2(validation$horsepower, pred, form = "traditional")
print(y)

#Random forest
fit.rf <- train(horsepower~., data=df, method="rf", metric=metric, trControl=control)
predictions1 <- predict(fit.rf, validation)
z = R2(validation$horsepower, predictions1, form = "traditional")
print(z)

#Decision Tree Regression
library(rpart)
fit.rpart <- train(horsepower~., data=df, method="rpart", metric=metric, trControl=control)
predictions2 <- predict(fit.rpart, validation)
a = R2(validation$horsepower, predictions2, form = "traditional")
print(a)

cat("Accuracy of SVM is",y,"\n","Accuracy of linear_regression is",x,"\n","Accuracy of random_forest_regression is",z,"\n","Accuracy of decision_regression is",a)

