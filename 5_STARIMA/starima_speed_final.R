rm(list = ls())

setwd("C:/Users/Chloe/Desktop/Msc UCL/Spatial Temp/final_data")
# install.packages("ape")
library(ape)
library(spdep)
library(raster)
library(dplyr)
library(ggplot2)
source("starima_package.R")

# Import both files
links <- read.csv("link_info.csv")
speed_data <- read.csv("traffic_speeds.csv")

# convert link_info file to matrix
link_info <- as.matrix(links)

#speed_data$day <- weekdays(as.Date(speed_data$Date))
#head(speed_data)

# +++++++++++++++++++++++++++++ Spatial Autocorrelation ++++++++++++++++++++++++++++++++++

#           ********* Compute Spatial Adjacency weight Matrix *****************

# We will compute weight matrix with distance
 # our link attributes will be important to compute distance 
# from one point to another.

# Compute distance from one point to another. NA warning message is okay.
dis <- dist(link_info)
dis

# Transform a distance matrix into a normal matrix.
D <- as.matrix(dis)

# spatial influence is often expressed as a continuous value, which we will need 
# when computing STARIMA, STACF and STPACF

# The simplest approach is to use inverse distance (the further away, the lower the value)
v <- 1 / D

# set diagonal value of matrix to 0
diag(v) <- 0
v
# w_link_matrix <- write.csv(v,"weight_links.csv", row.names = FALSE)

#                   ************ Compute Morans I **************
# Morans I is a measure of spatial autocorrelation. how related the values of a variable
# are based on the locations where they were measured.
# we are using ape library function Moran's I, because it's simple and fast

Moran.I(links$LCAP_ID, v)
#Based on these results, we can reject the null hypothesis that there is zero
# spatial autocorrelation in the variable LCAP_ID. we have confidence interval of 90%(z-value)

# ============= compute Moran's test with spdep library ====================
# we need to convert out weight matrix to list weight matrix
ww = mat2listw(v, row.names = NULL, style="W")
ww
moran(links$LCAP_ID, ww, n=length(ww$neighbours), S0=Szero(ww))

# Now we can test for significance. First analytically,
# using linear regression based logic and assumptions.
moran.test(links$LCAP_ID, ww, randomisation=FALSE)

# we can also compute monte-carlo simulation for moran's test
moran.mc(links$LCAP_ID, ww, nsim=99)

# convert to matrix and compute sum to confirm that weights add up to 1
mat <- listw2mat(ww)
apply(mat, 1, sum)[1:22]

# We can make a Moran scatter plot to visualize spatial autocorrelation.
# compute y to be used in plot, we are using length column. Any numeric column can be used.
y <- links$LENGTH

moran.plot(y, ww)
# Note the slop of regression line, our points are distributed linearly and scattered

# +++++++++++++++++++ weekdays and weekend pattern in traffic data ++++++++++++++++++++++++++++

# It is general assumption that traffic speed would be different in weekdays and weekend
# we will plot weekday and weekend on one variable to show if this is true or false

# convert date column as Date for plotting
speed_data$Date <- as.Date(speed_data$Date)

# plot the variables  
week_plot <- speed_data %>% #glimpse()
  mutate(date = as.Date(Date),
         day = format(as.Date(date), format = "%d"),
         weekdays = weekdays(as.Date(date)),
         weekend = (weekdays(Date)=="Saturday" | weekdays(Date)=="Sunday" ),
         Month = NULL) %>%
  # filter(date < as.Date("2019-02-01")) %>%
  ggplot(aes(date,X1745)) +
  geom_point() +
  theme_bw()+
  facet_grid(vars(weekdays),vars(weekend))
week_plot

# There are very interesting patterns in our data. 
# Highest speed can be seen on saturday, which crossed 0.75 threshold.
# but more interesting is wednesday, which is only marginally behind saturday.
# sundays traffic speed is even lower than weekdays.
# All other days are almost same. 


# *************** Trends, Seasonality and Cycles *****************************

# plot one variable 
plot(speed_data$X1747, ylab = " Speed ", xlab = "Count",
     type="l")

# It is clear from our plot that we have cyclical pattern in our data
# we have daily 180 observations,
# It is also common a assumption, that in evening, everyone is in a hurry to reach their destination. 

# we can plot first 150 observations to see if this is the case
plot(speed_data$X1747[1:150], ylab = " Speed ", xlab = "Observations Count",
     type="l")
# indeed, we see spikes/trend in our data

# plot from 181: 200
plot(speed_data$X1747[150: 300], ylab = " Speed ", xlab = "Observations Count",
     type="l")
# we can see that a cyclical trend is emerging.

# Now plot 1:1000 values
plot(speed_data$X1747[1: 1000], ylab = " Speed ", xlab = "Count",
     type="l")

# It is now clear that our data shows a cyclical pattern, 
# The highest spikes could point to either saturday or wednesday.
# Moreover our data has increasing and decreasing cycles on daily basis,
# we have to keep in mind when building our STARIMA model

# lag plot of our data
lag.plot(speed_data$X1745, lags=5, do.lines=FALSE)

# straight line is indication of linear dependence between variables,
# head of plot is quite dispersed. which means variables are not really dependent
# on one another

# ******** ACF and PACF plot **************
# plot acf and pacf on one variable
acf(speed_data$X1745,
    lag.max=180,
    xlab="Lag",
    ylab="ACF",
    main="Autocorrelation plot of monthly Traffic Speed")

# The data shows strong decreasing trend till 50 lags, then it became stabilized
# other lags point to cyclic daily behaviour (because we have 180 daily observations)

# try take differenced series to see if it makes any difference in ACF plot

speed.diff <- diff(speed_data$X1745, differences=2)
acf(speed.diff,
    lag.max=180,
    xlab="Lag",
    ylab="ACF",
    main="Differenced Autocorrelation plot of monthly Traffic Speed")

# lags difference is not useful, because our series doesn't have any seasonal
# pattern, but we have daily cyclic trend which is decreasing and increasing,
# so difference of two data points have proved to be very helpful


# Partial correlation plot
pacf(speed_data$X1745,
     lag.max=180,
     xlab="Lag",
     ylab="PACF",
     main="Partial Autocorrelation plot of monthly Traffic Speed")

# partial autocorrelation plot shows significant cyclic pattern
# which are gradually decreasing

# Differenced Partial Autocorrelation plot
pacf(speed.diff,
     lag.max=180,
     xlab="Lag",
     ylab="PACF",
     main="Partial Autocorrelation plot of monthly Traffic Speed")

# It is clear from both differenced and original series that our model is 
# auto regressive, so we will take our p value from original plot

#                  ============= SIMPLE ARIMA =====================
# Model Identification of ARIMA
# our MA (q) would be 1, because our series is not moving average, we can even try q=0,
# pacf is signifact at 1, p=1, spikes seems random
# time difference is d = 2, we are trying lag 12.
# Arima(1, 2, 1)(1, 1, 0)12 is our candidate. lets try it.

# Parameter Estimation and Fitting of ARIMA
fit.ar <- arima(speed_data$X1745[1:4500], order = c(1, 2, 1),
                seasonal = list(order=c(1, 1, 0), period=12))

fit.ar
# residuals
res <- fit.ar$residuals
NRMSE_fit <- NRMSE(res = fit.ar$residuals, obs = speed_data$X1745[1:4500])

# Normalised RMSE
NRMSE_fit

# Diagnostic checking
tsdiag(fit.ar)
Box.test(fit.ar$residuals, lag = 1)

# Step 06: prediction with an ARIMA model
pre.ar <- predict(fit.ar, n.ahead=24)
pre.ar
matplot(cbind(speed_data$X1745[4501:4524], pre.ar$pred), type = "l",
        main="ARIMA prediction effect")

# Our predictions does not look good. because we have not counted for other factor
# which are affecting traffic speed. So it has become important to try STARIMA

# +++++++++++++++++++ STACF and STPACF +++++++++++++++++++++++++++++++

# The above was an exploratory analysis, which has given us a clear 
# picture of our data, now we will move to STACF and STPACF

# remove data and time columns so we can compute STARIMA
# and convert data to matrix
speed_mat <- as.matrix(subset(speed_data, select = -c(Time, Date)))
 
# speed data obs are in unit journey times in seconds per metre.
# convert them to speed by taking the inverse
speed_mat <- 1/ speed_mat
speed_mat
# and weight matrix
w_mat <- v
colnames(w_mat) <- colnames(speed_mat) # convert weight matrix name as speed_mat names


# Step 01: Space-time Autocorrelation and partial autocorrelation analysis

stacf(speed_mat, w_mat, 300)
# Auto correlation plot is linear, either it's decreasing or increasing

# Space time partial autocorrelation plot

stpacf(speed_mat, w_mat, 50)
# There are interesting patterns in STPACF. First lag is important
# and from second to almost 32 lags we have negative trend
# and then other 32 lags are going in upward direction


#           ++++++++++ Differencing and Removal of trend/cyclical pattern +++++++++++

# Step 02: Now we will remove trend and cycles
# we are taking lag 2 because our series does not contain seasonality, only cycles
speed_mat.diff <- diff(speed_mat, lag = 2)
stacf(speed_mat.diff, w_mat, 30)
stpacf(speed_mat.diff, w_mat, 30)

# It is clear from graph that lag has removed stationarity from auto correlation plot.
# partial autocorrelation is still showing negative and positve trend, we have to use this 
# information in our STARIMA model


#          ++++++++++++++++++++++ STARIMA Model +++++++++++++++++++++++++++++

# split data in train and test
training_data <- speed_mat[1:4141,] # we are taking first 23 days data as training for now
test_data <- speed_mat[4131:5400]   # last 7 days for test

# Step 03: Model Identification of STARIMA

# It is very clear from our analysis that we have an autoregressive model, which mean
# The value of p is very important and we need to count for time differene (d). which
# can be deduced from Partial autocorrelation plot.
# we can take either p as 1 or 2, and q as 1, as it is only
# significant at this value after differencing. we will take d as 2
# so we are building (2, 2, 1)model.

# Step 04: Parameter Estimation and Fitting of STARIMA
# convert waits to list
W_fit <- list(w1=w_mat)
fit.star <- starima_fit(speed_mat[1:4141,], W_fit, p=2, d=2, q=1)

# Step 05: Diagnostic Checking
# we can use STACF to check the residuals.
stacf(fit.star$RES, w_mat, 48)

# histogram of residuals
hist(fit.star$RES[,6], main= "histogram of residuals")

# Prediction with the STARIMA model
pre.star <- starima_pre(speed_mat[(4141-12-4+1):5400,], model = fit.star)

# check predictions against actual values
matplot(cbind(speed_mat[4131:5400,1], pre.star$PRE[,1]), type="l")

# Calculate RMSE for Our Model 
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

RMSE(pre.star$PRE[,1], speed_mat[4131:5400,1])

# We can also compute normalize RMSE

# normalized residual errors
pre.star$NRMSE

# It is giving us residual errors for every variables total 22
# we can sum up to see total normalised RMSE 
sum(pre.star$NRMSE)

# +++++++++++++++ Using previous timesteps to predict upcoming timesteps +++++++++++++++++++

# STARIMA model predictions are quite good and it can be used to 
# predict 7 days ahead. As a start we can predict daily observations
# Using almost 180 daily observations


# ++++++++++++++++++++ Second STARIMA Model ++++++++++++++++++++

# previously we built arima model with p = 2, d = 2, and q = 1. It is also possible that 
# we can try p = 2, d = 1 and q = 1.
# We are also going to change our training and test data ratio, to see
# if it makes any difference to model 

fit.star2 <- starima_fit(speed_mat[1:5000,], W_fit, p=2, d=1, q=1)

# stacf of residulas 
stacf(fit.star2$RES, w_mat, 48)

# histogram of residuals
hist(fit.star2$RES[,6])

# Prediction with the STARIMA 2 model
pre.star2 <- starima_pre(speed_mat[(5000-12-4+1):5400,], model = fit.star2)
pre.star2
# Compute plot for second variable
matplot(cbind(speed_mat[4989:5400,2], pre.star2$PRE[,2]), type="l")

# Compute RMSE 
RMSE(pre.star2$PRE[,2], speed_mat[4989:5400,2])

# normalized residual error
pre.star2$NRMSE
sum(pre.star2$NRMSE)
# ++++++++++++++++++++ Conclusion ++++++++++++++++++++++++

# both models can be considered a good fit to data, but I think the model with STARIMA(2, 1, 1)
# is better than second model. Looking at graph we can see that predicted observations are much 
# closer to actual observations.






