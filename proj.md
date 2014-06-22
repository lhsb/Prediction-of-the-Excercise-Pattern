Prediction of the Excercise Pattern
========================================================

## Loading liabraries and data


```r
require(gbm)

# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
#              destfile="~/training.csv",method='wget')
training = read.csv("~/training.csv")
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
#               destfile="~/testing.csv",method='wget')
testing = read.csv("~/testing.csv")
```

## Explore training dataset

* observation vs. variables ... as is
* observation vs. variables without NAs
* number of columns which contains NA
* all those columns contain the same amount of NAs 19216 which is almost 98% of all observations
 * there is no racional reason to impute the data


```r
dim(training)
```

```
## [1] 19622   160
```

```r
dim(na.omit(training))
```

```
## [1] 406 160
```

```r
colSumsNA = colSums(is.na(training))
sum(colSumsNA > 0)
```

```
## [1] 67
```

```r
all(colSumsNA[colSumsNA>0] == 19216)
```

```
## [1] TRUE
```

## Clean training dataset

* __remove variables:__ 'X', 'cvtd_timestamp', all which conatin NA and empty observation record


```r
training = training[, -c(1, 5,
                         which(colSums(is.na(training)) > 0),
                         which(colSums(training == "") > 0))]
```

## Transform training dataset variables

* transform factor varibales into binary variable


```r
factCols = c(1,4)
for(colt in factCols) {    
    d = as.data.frame(training[,colt])
    for(i in levels(d[,1])) {
        d = cbind(d, (i == d[,1]) * 1)
        colnames(d)[ncol(d)] = paste0(colnames(training)[colt],"_",i)
    }
    training = cbind(training, d[,-1])
}
training = training[,-c(factCols)]
d = training[,57:64]
d[d == 0]=-1
training[,57:64] = d
```

## Create model

* I choosed Gradient Boosting Method for multinomial distribution
 * it provides very high accuracy but interpretability is bad
 * this kind of method is based on boosting trees
* shown configuration is underfitted - see the picture
 * I recommend increase n.tree parameter - it improves accuracy
 * I recommend increase cv.fold parameter - it improves robust of the model


```r
set.seed(123)

fitMod <- gbm(classe ~ .,
           data = training,
           distribution = "multinomial",
           n.tree = 2000,
           shrinkage = 0.001,
           cv.folds = 4,
           bag.fraction = 0.8,
           interaction.depth = 3,
           verbose = FALSE)
gbm.perf( fitMod, method="cv" )
```

![alt Multinomial Deviance](figure/multinomial_deviance.png)

## Prepare testing data set

* __remove variables__: 'X', 'cvtd_timestamp', 'problem_id', all clumns with NAs
* transform factorial variables to the binary variables
* all variables in the test set must have the same format as in training dataset


```r
testing = testing[,-c(which(colnames(testing) == "problem_id"))]
testing = testing[, -c(1, 5, which(colSums(is.na(testing)) > 0))]
factCols = c(1)
for(colt in factCols) {    
    d = as.data.frame(testing[,colt])
    for(i in levels(d[,1])) {
        d = cbind(d, (i == d[,1]) * 1)
        colnames(d)[ncol(d)] = paste0(colnames(testing)[colt],"_",i)
    }
    testing = cbind(testing, d[,-1])
}
testing = testing[,-c(factCols)]
testing$new_window_no = 1
testing$new_window_yes = 0
testing = testing[,-c(which(colnames(testing) == "new_window"))]
d = testing[,56:63]
d[d == 0]=-1
testing[,56:63] = d
```

## Predict and prepare output for evaluation

* there is 80% accuracy on the test data set


```r
pr = predict(fitMod, testing, type="response")

answers = apply(pr, 1, function(x) colnames(pr)[which.max(x)])
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(answers)
```
