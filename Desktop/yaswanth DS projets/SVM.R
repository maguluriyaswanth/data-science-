data= read.csv("c:/Users/maguly/Desktop/Project/dataset.csv", header = T)

## splitting sample data for 10000 rows 

library(dplyr)
sample = sample_n(data, 40000)
##to find the standed devation is zero
t(sapply(sample, sd)==0)
##removing standed devation  
sample <- sample[,c(-67)]
sample <- sample[,c(-94,-95)]

## replacing missing values in to 999
sample = mutate_all(sample, funs(replace(., is.na(.), 999)))

partdata <- sample(2,nrow(sample), replace =TRUE, prob =c(0.8,0.2) )

training  <- sample[partdata==1,]
validation <- sample[partdata==2,]

set.seed(100)

library(e1071)

### applying SVM model tpo the data 
##model = svm(y ~., data = training)



svm_regressor = svm(formula = y ~ .,
                    data = training,
                    type = 'eps-regression',
                    kernel = 'radial')

###predict output
##preds <- predict(model, validation)

svm_pred = predict(svm_regressor, newdata = validation)


aaa= table(svm_pred)

View(aaa)

correlation = cor(svm_pred, validation$y)

#Finding accuracy of the model using correlation

correlation_accuracy <- cor(actuals_preds)
##[1] 0.01650481
print(correlation_accuracy)
rmse <- sqrt(mean(svm_pred-validation$y)^2)
##18.59121
meanobsulateerror = mean(abs(svm_pred - validation$y)/svm_pred)
## 0.1655826
