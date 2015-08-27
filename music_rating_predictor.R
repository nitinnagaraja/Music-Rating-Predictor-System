trainData <- read.csv('data/train.csv');
testData <- read.csv('data/test.csv');
wordsData <- read.csv('data/words.csv');
usersData <- read.csv('data/users.csv');
library(randomForest)

cleantimes <- function (t) {
  t <- as.character(t)
  t[t=='Less than an hour'] <- '.5';
  t[t=='More than 16 hours'] <- '18';
  t <- as.numeric(substr(t, 1, 2));
  return(t)
}
usersData$LIST_OWN <- cleantimes(usersData$LIST_OWN);
usersData$LIST_BACK <- cleantimes(usersData$LIST_BACK);


# function to merge train\test data, words and users.
mergeData <- function (dataSet) {
  dataSet$RowID <- 1:nrow(dataSet);
  mergedSet <- merge(dataSet, wordsData, all.x=T);
  mergedSet <- merge(mergedSet, usersData, by.x='User', by.y='RESPID', all.x=T)
  mergedSet <- na.roughfix(mergedSet);
  mergedSet <- mergedSet[order(mergedSet$RowID),];
  mergedSet$RowID <- NULL;
  return(mergedSet);
}

wordsData$X <- NULL;
rowsData <- is.na(wordsData$Good.Lyrics)
wordsData$Good.Lyrics[rowsData] <- wordsData$Good.lyrics[rowsData];
wordsData$Good.lyrics <- NULL;
wordsData$OWN_ARTIST_MUSIC <- as.character(wordsData$OWN_ARTIST_MUSIC);
wordsData$OWN_ARTIST_MUSIC[substr(wordsData$OWN_ARTIST_MUSIC, 2, 2) == 'o'] <- 'DK';
wordsData$OWN_ARTIST_MUSIC <- as.factor(wordsData$OWN_ARTIST_MUSIC);

# preprocessing heard_of values to reduce the number of distinct values.
wordsData$HEARD_OF[wordsData$HEARD_OF == ''] <- 'Never heard of';
wordsData$HEARD_OF[wordsData$HEARD_OF == 'Ever heard music by'] <- 'Heard of and listened to music EVER';
wordsData$HEARD_OF[wordsData$HEARD_OF == 'Ever heard of'] <- 'Heard of';
wordsData$HEARD_OF[wordsData$HEARD_OF == 'Listened to recently'] <- 'Heard of and listened to music RECENTLY';
wordsData$HEARD_OF <- droplevels(wordsData$HEARD_OF);
# random forest module to handle NA values.
wordsData <- na.roughfix(wordsData);

# splitting 50% the training data to test and train.
trainIndexValue=sample(1:nrow(trainData),nrow(trainData)/2)
testData=trainData[-trainIndexValue,]
trainData=trainData[trainIndexValue,]
trainfeatsData <- mergeData(trainData)[,-4]; #merging words users and train data

testfeatsData <- mergeData(testData); #merging words, users and test data

#creating a list consisting of training, test and ratings of training data.
cleanedData <- list(trainfeatsData=trainfeatsData, testfeatsData=testfeatsData, ratingsData=trainData$Rating)

library(gbm);
library(randomForest);
library(compiler);
library(plyr);

enableJIT(3);
numUser <- 50928;
numTrack <- 184;

rmseError <- function (a, b) mean((a - b)^2)^0.5;

trainfeatsData <- cleanedData[[1]];
testfeatsData <- cleanedData[[2]];
ratingsData <- cleanedData[[3]];
#prune rf

#rf <- randomForest(trainfeatsData, ratingsData, do.trace=T, sampsize=50000, ntree=1);

#pruning the 40 most important attributes using random forest
#attributes= importance(rf)
#ordered_attributes=attributes[order(attributes),]
#prunedData=ordered_attributes[(length(ordered_attributes)-40):length(ordered_attributes)]

#random forest
rfpred <- function (trainfeats, testfeats, ratings) {    
  rf <- randomForest(trainfeats, ratings, do.trace=T, sampsize=50000, ntree=100);
  
  #pruning the 40 most important attributes using random forest
  attr= importance(rf)
  ordered_attr=attr[order(attr),]
  pruned=ordered_attr[(length(ordered_attr)-40):length(ordered_attr)]
  
  #rerunning random forest with 40 important attributes
  rf <- randomForest(trainfeats[,names(pruned)], ratings, do.trace=T, sampsize=50000, ntree=100);
  pred <- predict(rf, testfeats);     #predicting test data ratings with the model
  cv <- rf$predicted;                 #saving training predicted values
  
  return(list(pred=pred, cv=cv));
}


#random forest applied to each artist.
rfbyartistpred <- function (train, test, ratings) {
  train$Rating <- ratings;
  #splitting dataset on basis of artist and building random forest for each such data set
  rfs <- dlply(train, .(Artist), function (x) {
    rating <- x$Rating;
    x$Rating <- NULL;
    return(randomForest(x, rating, ntree=100, nodesize=10, do.trace=F));
  }, .progress='text');
  pred <- numeric(nrow(test));
  for (i in 0:max(test$Artist)) {
    ids <- test$Artist == i;
    if (sum(ids) > 0) pred[ids] <- predict(rfs[[as.character(i)]], test[ids,]);
  }
  return(list(pred=pred));
}

#Linear model applied to each artist.
lmbyartistpred <- function (train, test, ratings) {
  
  #data preprocessing to convert all factors to numeric
  isf <- which(sapply(1:ncol(train), function (n) is.factor(train[,n])));
  for (i in isf) {
    train[,i] <- as.integer(train[,i]);
    test[,i] <- as.integer(test[,i]);
  } 
  train$Rating <- ratings;
  
  #splitting by artist
  lms <- dlply(train, .(Artist), function (x) {
    rating <- x$Rating;
    x$Rating <- NULL;
    return(lm(rating ~ ., data=x));
  }, .progress='text');
  pred <- numeric(nrow(test));
  for (i in 0:max(test$Artist)) {
    ids <- test$Artist == i;
    if (sum(ids) > 0) pred[ids] <- predict(lms[[as.character(i)]], test[ids,]);
  }
  return(list(pred=pred));
}

#function to perform cross validation of predicted values for train data.
cross.val <- function (predictor, folds, trainfeats, testfeats, ratings, ...) {
  cat('Running primary predictor\n');
  pred <- predictor(trainfeats, testfeats, ratings, ...);
  items <- sample(folds, nrow(trainfeats), T);
  cv <- numeric(nrow(trainfeats));
  for (i in 1:folds) {
    samp <- which(items == i);
    cat('Running CV fold', i, '\n');
    cpred <- predictor(trainfeats[-samp,], trainfeats[samp,], ratings[-samp], ...);
    cat('Estimated RMSE for fold:', rmseError(ratings[samp], cpred$pred), '\n');
    cv[samp] <- cpred$pred;
  }
  pred$cv <- cv;
  return(pred);
}

#Linear model for the whole data set with all attributes.
lmpred <- function (train, test, ratings) {
  l <- lm(ratings ~ ., data=trainfeatsData); 
  pred <- predict(l, test); 
  return(list(pred=pred))
}

#gbm model 
gbmpred <- function (train, test, ratings) {
  gb <- gbm.fit(trainfeatsData, ratings, distribution='gaussian', shrinkage=0.08, n.trees=250, interaction.depth=10);
  pred <- predict(gb, test, n.trees=250);
  return(list(pred=pred))
}

#running random forest based on only the questions answered. - to be removed as we are 
#pruning the tree using rf.
rfqpred <- function (train, test, ratings) {
  train <- train[,c(2,3,96:114)];
  test <- test[,c(2,3,96:114)];
  rf <- randomForest(train, ratings, do.trace=T, sampsize=50000, ntree=200);
  pred <- predict(rf, test);
  cv <- rf$predicted;
  return(list(pred=pred, cv=cv));
}

#function to write predicted values to a file.
save.pred <- function (name, pred) {
  cat('Estimated RMSE: ', rmseError(ratingsData, pred$cv), '\n');  
  filename <- paste('predictions/', name, '.csv', sep='');  
  write.csv(pred$pred, filename, row.names=F, quote=F);
  write.csv(pred$cv, paste(filename, '.cross', sep=''), row.names=F, quote=F);
}

#Run different models and write the predictions to appropriate files under predictions folder.
save.pred('lmbya', cross.val(lmbyartistpred, 10, trainfeatsData, testfeatsData, ratingsData));
save.pred('rfbya', cross.val(rfbyartistpred, 10, trainfeatsData, testfeatsData, ratingsData));
save.pred('rf', rfpred(trainfeatsData, testfeatsData, ratingsData));
save.pred('gbm', cross.val(gbmpred, 10, trainfeatsData, testfeatsData, ratingsData));
save.pred('lm', cross.val(lmpred, 10, trainfeatsData, testfeatsData, ratingsData));


#interpreting results
interpret <- function(filename){
  predicted=read.csv(paste("predictions/",filename,sep=""),header=T)
  predicted=as.numeric(predicted[,1])
  rmse_error=rmseError(predicted,testData$Rating)
  #rsq_error=rsq(testData$Rating,predicted)
}
b=sample(a,10)
barplot(b, xaxt = "n", xlab='Some Letters',type="h")
axis(1, at=1:length(b), labels=names(b))
wordcloud(names(a),a-min(a))
library("wordcloud")
files=c("gbm.csv","knn.csv","lm.csv","lmbya.csv","rf.csv","rfbya.csv")
lapply(files,interpret)
