
## Abstract 
 I used the random forest method to estimate features for the Human Activity Recognition data set from Groupware. I divided the training data set into two samples with 70% cases going to the training_training data set, and 30% goinng to the training_testing data set. This allowed me to test my model before predicting the actual test. I got 0.998 accuracy.
 
 
 
 
## Data 
The data is taken from the [Human Activity Recognition](http://groupware.les.inf.puc-rio.br/har) program at [Groupware](http://groupware.les.inf.puc-rio.br/). 

 
 ```r
 library(caret)
 training <- read.csv("pml-training.csv")
 testing <- read.csv("pml-testing.csv")
 ```

The data set includes many columns with variables with NAs. I am removing them from my analysis.

 
 ```r
 columnnames <- names(training)
 training2 <- training[, -grep("kurtosis_", columnnames)]
 columnnames <- names(training2)
 training3 <- training2[, -grep("skew", columnnames)]
 columnnames <- names(training3)
 training4 <- training3[, -grep("min_", columnnames)]
 columnnames <- names(training4)
 training5 <- training4[, -grep("max_", columnnames)]
 columnnames <- names(training5)
 training6 <- training5[, -grep("amplitude_", columnnames)]
 columnnames <- names(training6)
 training7 <- training6[, -grep("var_", columnnames)]
 columnnames <- names(training7)
 training8 <- training7[, -grep("avg_", columnnames)]
 columnnames <- names(training8)
 training9 <- training8[, -grep("stddev_", columnnames)]
 training10 <- training9[, -c(1, 2, 6, 7)]
 ```

I also decided not to use user_name, new_window and num_window variables in my model.

## Method 
I use the Random Forests method. I also divided my training set into training and testing sets in order to be able to test my prediction before submiting my prediction for 20 test cases. 
 
 ```r
 set.seed(1111)
 inTrain <- createDataPartition(y = training10$classe, p = 0.7, list = FALSE)
 training_training <- training10[inTrain, ]
 training_testing <- training10[-inTrain, ]
 
 
 modfit <- train(as.factor(training_training$classe) ~ ., method = "rf", data = training_training)
 ```
 
 ```
 ## Loading required package: randomForest
 ## randomForest 4.6-10
 ## Type rfNews() to see new features/changes/bug fixes.
 ```




## Results 
I can compare my predictions of my test cases I created with the true values.

```r
prediction <- predict(modfit, training_testing)
table(prediction, training_testing$classe)
```

```
##           
## prediction    A    B    C    D    E
##          A 1673    1    0    0    0
##          B    1 1134    1    0    0
##          C    0    4 1022    2    0
##          D    0    0    3  962    0
##          E    0    0    0    0 1082
```

We can see that my predictions are rather accurate as most cases are located on the diagonal of the table. However, we can look into some statistics to see how accurate these predictions are.


```r
confusionMatrix(prediction, training_testing$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1673    1    0    0    0
##          B    1 1134    1    0    0
##          C    0    4 1022    2    0
##          D    0    0    3  962    0
##          E    0    0    0    0 1082
## 
## Overall Statistics
##                                         
##                Accuracy : 0.998         
##                  95% CI : (0.996, 0.999)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.997         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.999    0.996    0.996    0.998    1.000
## Specificity             1.000    1.000    0.999    0.999    1.000
## Pos Pred Value          0.999    0.998    0.994    0.997    1.000
## Neg Pred Value          1.000    0.999    0.999    1.000    1.000
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.284    0.193    0.174    0.163    0.184
## Detection Prevalence    0.284    0.193    0.175    0.164    0.184
## Balanced Accuracy       1.000    0.998    0.997    0.999    1.000
```

 
 Accuracy is high (0.998) with a very small confidence interval (0.996,0.999), which means I can be fairly certain of my predictions and my out of sample error is low.
 
 
 
 ```r
 plot(varImp(modfit))
 ```
 
 ![plot of chunk plot](figure/plot.png) 



Finally, I need predictions for the test cases in the assignment.
I am subjecting the testing data set to the same manipulations I done with the training data set. (I got 20/20 when I submited my predictions.)

```r
columnnames <- names(testing)
testing2 <- testing[, -grep("kurtosis_", columnnames)]
columnnames <- names(testing2)
testing3 <- testing2[, -grep("skew", columnnames)]
columnnames <- names(testing3)
testing4 <- testing3[, -grep("min_", columnnames)]
columnnames <- names(testing4)
testing5 <- testing4[, -grep("max_", columnnames)]
columnnames <- names(testing5)
testing6 <- testing5[, -grep("amplitude_", columnnames)]
columnnames <- names(testing6)
testing7 <- testing6[, -grep("var_", columnnames)]
columnnames <- names(testing7)
testing8 <- testing7[, -grep("avg_", columnnames)]
columnnames <- names(testing8)
testing9 <- testing8[, -grep("stddev_", columnnames)]
testing10 <- testing9[, -c(1, 2, 6, 7)]

prediction2 <- predict(modfit, testing10)


answers <- prediction2
pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
            col.names = FALSE)
    }
}

pml_write_files(answers)
```

