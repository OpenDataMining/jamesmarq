#load required packages
require(SASxport)
require(FSelector)
require(caret)
require(MASS)
require(Metrics)

#Read the data from AHRQ, unzip, and convert to dataframe
temp <- tempfile()
download.file("http://meps.ahrq.gov/mepsweb/data_files/pufs/h156ssp.zip",temp)
con <- unzip(temp, "h156.ssp")
meps.2011.2012 <- read.xport("h156.ssp")

# ----
# Read the documentation on all the variables here - http://meps.ahrq.gov/mepsweb/data_stats/download_data/pufs/h156/h156doc.pdf
# ----

#scrub the data - omit null values, charge values of less than 0
meps.2011.2012 <- na.omit(meps.2011.2012)
meps.2011.2012 <- meps.2011.2012[meps.2011.2012$TOTTCHY2 >=0 & meps.2011.2012$TOTTCHY1 >=0,]

#Rank variables using pearsons, discard those not above critical value  
importance<-linear.correlation(TOTTCHY2~., meps.2011.2012[,sapply(meps.2011.2012,function(x) is.integer(x) | is.numeric(x))])
importance$name<-row.names(importance)
crit.value<-0.103
vars<-importance[importance$attr_importance>=crit.value,]$name
meps.2011.2012.scrubbed<-meps.2011.2012[,c(na.omit(vars),"TOTTCHY2")]

#set up fit control for 10 fold cross validation
fitControl <- trainControl(method = "cv", number = 3)

#Fit three regression models (regression tree, m5 trees, ridge regression), select best according to RMSE
sample.ind<-sample(1:nrow(meps.2011.2012.scrubbed),1000)
rpart.fit<- train(TOTTCHY2~., data = meps.2011.2012.scrubbed[sample.ind,], preProcess=c("range"), method = "rpart", trControl = fitControl, tuneLength=5, metric="RMSE")
cubist.fit<- train(TOTTCHY2~., data = meps.2011.2012.scrubbed[sample.ind,], preProcess=c("range"), method = "cubist", trControl = fitControl, tuneLength=5, metric="RMSE")
foba.fit<- train(TOTTCHY2~., data = meps.2011.2012.scrubbed[sample.ind,], preProcess=c("range"), method = "foba", trControl = fitControl, tuneLength=5, metric="RMSE")
baseline.fit<-RMSE(pred = meps.2011.2012.scrubbed$TOTTCHY1*1.1, obs = meps.2011.2012.scrubbed$TOTTCHY2)

#compare the models and see which is the best
results <- resamples(list(rpart=rpart.fit, cubist=cubist.fit, foba=foba.fit))
baseline.fit
summary(results)
bwplot(results)

#----Classification----#

#add new feature to indicate increase / decrease in cost, and drop cost feature
meps.2011.2012.scrubbed$costincrease<-as.factor((meps.2011.2012.scrubbed$TOTTCHY2-meps.2011.2012.scrubbed$TOTTCHY1) > 0)
cost.2012<-meps.2011.2012.scrubbed$TOTTCHY2
meps.2011.2012.scrubbed$TOTTCHY2<-NULL

#Fit three classification models (classification tree, logistic regression, ), select best according to Accurary
sample.ind<-sample(1:nrow(meps.2011.2012.scrubbed),10000)
rpart.fit<- train(costincrease~., data = meps.2011.2012.scrubbed[sample.ind,], preProcess=c("range"), method = "rpart", trControl = fitControl, tuneLength=5, metric="Accuracy")
logit.fit<- train(costincrease~., data = meps.2011.2012.scrubbed[sample.ind,], preProcess=c("range"), method = "LogitBoost", trControl = fitControl, tuneLength=5, metric="Accuracy")
svm.fit<- train(costincrease~., data = meps.2011.2012.scrubbed[sample.ind,], preProcess=c("range"), method = "svmLinear", trControl = fitControl, tuneLength=5, metric="Accuracy")

#compare the models and see which is the best
results <- resamples(list(rpart=rpart.fit, logit=logit.fit, svm=svm.fit))
summary(results)
bwplot(results)