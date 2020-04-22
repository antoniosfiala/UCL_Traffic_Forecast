
# LOAD DATA AND PACKAGES --------------------------------------------------

setwd("C:/Users/hanna/Desktop/Winter2020/spatio-temporal/assessment/analysis/UCL_Traffic_Forecast")
load("./0_source_data/UJTWorkSpace_Processed.RData")
library(spdep)
library(caret)
library(reshape2)

# SPATIAL WEIGHT MATRIX ---------------------------------------------------

# subset the adjacency matrix 
names <- as.character(Subsetted$LCAP_ID) # names of the selected links 
adj <- LCAPAdj[c(names),c(names)] # get the right columns and rows

# do row standardization (sum of all rows should be 1) and transform to 'nb' object
sums <- rowSums(adj) # get sums for each row
StandAdj <- adj # copy the matrix
for (i in 1:length(sums)){
  if(sums[[i]]>0){
    StandAdj[i,]<-StandAdj[i,]/sums[[i]]
  }
} # do the standardization

StandAdj <- as.data.frame(StandAdj) # convert to dataframe 


# MODEL PARAMETERS --------------------------------------------------------

m <- 3 # number of time lags
C <- 10 # amount of 'error' allowed
sigma <- 0.1 # smoothing parameter for RBF kernel 
training <- 0.8 # percentage of data used for training
link <- '4' #LCAP_ID for the link used for model
e <- 0.01 # epsilon parameter

Sigmas <- 2^seq(-11,1,by=2)
Cs <- 2^seq(3,15,by=2)
Es <- c(0.001, 0.01, 0.1)

# PROCESS THE DATA --------------------------------------------------------

traffic_speeds <- Subsetted[,1:5400] %>% st_drop_geometry() # get speeds from subsetted data
traffic_speeds <- as.data.frame(t(traffic_speeds)) # transpose
rownames(traffic_speeds) <- c() # remove row names 

data_ <- embed(x=traffic_speeds[,link], dimension=m+1) # embed to format with m number of time lags
colnames(data_) <- c("y_t-3","y_t-2","y_t-1","y_t") # rename the columns according to lags

split_ <- ceiling(nrow(data_)*training) # get the point of splitting test and train  

# get test and train data
yTrain_ <- data_[1:split_,1]
XTrain_ <- data_[1:split_,-1]
yTest_ <- data_[(split_+1):nrow(data_),1]
XTest_ <- data_[(split_+1):nrow(data_),-1]


# FIT THE MODEL -----------------------------------------------------------

# link to example with epsilon training: https://stackoverflow.com/questions/27828120/error-with-custom-svm-model-for-tuning-in-caret
# https://stackoverflow.com/questions/43019667/error-with-tuning-custom-svm-model-in-caret
# also k fold with time series: https://stackoverflow.com/questions/24758218/time-series-data-splitting-and-model-evaluation 
#timectrl <- trainControl(method = 'timeslice', initialWindow = )

ctrl <- trainControl(method = "cv", number=5) # 5-fold cross validation
SVRGrid <- expand.grid(.sigma=Sigmas, .C=Cs) # make a grid of parameter combos to test 
system.time(SVRFit_Traffic <- train(XTrain_, yTrain_, method="svmRadial", tuneGrid=SVRGrid
                      , trControl=ctrl, type="eps-svr", eps=e)) # test models with the diff parameters 

SVRFit_Traffic # get a summary of the fitting procedure (can see RMSE and MAE) 
plot(SVRFit_Traffic) # compare model performance across diff hyperparameters 
SVRFit_Traffic$finalModel # look at the final model

print(SVRFit_Traffic$finalModel)

saveRDS(SVRFit_Traffic, "model_4.rds")
my_model_2 <- readRDS("4_SVR/model_2.rds")


# VISUALIZE FITTING RESULTS -----------------------------------------------

results <- SVRFit_Traffic$results[,c(1,2,3)] # get subset of results with just RMSE
test <- acast(results, sigma~C, valu.var='RMSE') # reshape to appropriate format
levelplot(test, ylab='C', xlab='sigma') # make heatmap to show best parameter combinations 

# PREDICT AND VISUALIZE ---------------------------------------------------

yPred_ <- predict(SVRFit_Traffic, XTest_) # predict y values from the test x values
# create graph comparing observed and predicted UKTemp values 
plot(yTest_[1:200], type="l", xaxt="n", xlab="Time", ylab="Traffic Speeds")
points(yPred_[1:200], col="blue", pch=15, bg="blue")
points(data_[SVRFit_Traffic@alphaindex,c("x", "y")], cex=2, pch=21, col="black", bg="black")
axis(1, at=seq(90, (180*5)+90, 180), labels=unique(dates[(split_+1):nrow(data_),1]))
#axis(1, at=seq(6, (12*19)+6, 12), labels=format(dates[(trainYears+1):length(dates)], "%Y"))
#legend(90, 3, legend=c("Observed"), lty=1, bty="n")
#legend(150, 3, legend=c("Predicted"), pch=21, col="blue", bg="blue", bty="n")

plot(yTrain_, type='l')
plot(yTest_, type='l')
# POST FIT DIAGNOSTICS ----------------------------------------------------


# check for temporal autocorrelation in the model residuals 
SVRResidual <- yTest_-yPred_
plot(SVRResidual)

acf(SVRResidual) # shows that some significant temporal autocorrelation remains

