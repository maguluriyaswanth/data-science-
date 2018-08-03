##dacisiontree 

##To partination the data
partdata <- sample(2,nrow(sample), replace =TRUE, prob =c(0.8,0.2) )
training  <- sample[partdata==1,]
validation <- sample[partdata==2,]
#Decision tree with party
library(party)
mytree <- ctree(y ~., data=training, controls=ctree_control(mincriterion=0.99, minsplit=100))
print(mytree)
plot(mytree, col = rainbow(7))

t(sapply(validation, sd)==0)

## pridect 
library(e1071)

desction = predict(mytree, validation)

bbb= table(desction)

#Finding accuracy of the model using correlation
desction.correlation_accuracy <- cor(desction, validation$y)
print(desction.correlation_accuracy)
##0.9372929

rmse <- sqrt(mean(desction - validation$y)^2)
##0.008038078

meanobsulateerror = mean(abs(desction - validation$y)/desction)

###0.05052706
