# oblig 2 - problem 2

library(kknn)
library(splines)
library(fANCOVA)

# read in, inspect and prep data
path <- file.path(getwd() , "res_bodyfat.csv")
bodyfat <- read.csv(path)
summary(bodyfat)
head(bodyfat)
attach(bodyfat)

# a) 

# do local regression with different spans (0.15,0.5 & 1)
X_0 = seq(min(bmi), max(bmi), length=250)

# plot points without model
par(mfrow=c(2,2))
plot(bmi, pbfm, cex =0.2, main= "Data")

# fit model with span 0.15
plot(bmi, pbfm, cex =0.2, main= "Span: 0.15")
lr_015 = loess(pbfm~bmi, span=0.15, data = bodyfat)
lines(x = X_0, y = predict(lr_015, newdata = X_0), col = "4", lwd=2)

# fit model with span 0.5
plot(bmi, pbfm, cex =0.2, main= "Span: 0.5")
lr_05 = loess(pbfm~bmi, span=0.5, data = bodyfat)
lines(x = X_0, y = predict(lr_05, newdata = X_0), col = "2", lwd=2)

# fit model with span 1
plot(bmi, pbfm, cex =0.2, main= "Span: 1")
lr_1 = loess(pbfm~bmi, span=1, data = bodyfat)
lines(x = X_0, y = predict(lr_1, newdata = X_0), col = "3", lwd=2)

# b)

# fit and plot local regression model (when not setting user.span, it is chosen to the optimal)
lr_best = loess.as(pbfm, bmi, criterion="gcv", plot=TRUE)

#look at summary to find optimal span
summary(lr_best)

# c)

par(mfrow=c(3,1))
bmi_range =range(bmi)
X_sequence = seq(from =bmi_range[1], to= bmi_range[2])

#fit regression spline models of different orders (1,2,3)
spline_model_1 = lm(pbfm ~ bs(bmi, df=4, degree=1), data=bodyfat )
spline_model_2 = lm(pbfm ~ bs(bmi, df=4, degree=2), data=bodyfat )
spline_model_3 = lm(pbfm ~ bs(bmi, df=4, degree=3), data=bodyfat )

# predict using the spline models
pred_smodel_1 = predict(spline_model_1, newdata = list(bmi=X_sequence), se=T)
pred_smodel_2 = predict(spline_model_2, newdata = list(bmi=X_sequence), se=T)
pred_smodel_3 = predict(spline_model_3, newdata = list(bmi=X_sequence), se=T)

# plot result of the three predictions
plot(bmi, pbfm, main = "spline 1")
lines(X_sequence, pred_smodel_1$fit, col="green", lwd=2)
plot(bmi, pbfm, main = "spline 2")
lines(X_sequence, pred_smodel_2$fit, col="blue", lwd=2)
plot(bmi, pbfm, main = "spline 3")
lines(X_sequence, pred_smodel_3$fit, col="red", lwd=2)

# d)
X_0 = seq(min(bmi), max(bmi), length = 250)

# fit smoothing spline models, with varying smoothing parameters (manual vs using loocv)
smooth_model_025 = smooth.spline(bmi, pbfm, spar = 0.25)
smooth_model_05 = smooth.spline(bmi, pbfm, spar = 0.5)
smooth_model_125 = smooth.spline(bmi, pbfm, spar = 1.25) 
smooth_model_auto = smooth.spline(bmi, pbfm, cv=TRUE) 

#get smoothing paramter value chosen through loocv
smooth_model_auto$spar

# plot all fitted models
par(mfrow=c(1,1))
plot(bmi, pbfm, main = "Smoothing Spline Models")
lines(predict(smooth_model_025, x = X_0), col="green", lwd=2)
lines(predict(smooth_model_05, x = X_0), col="red", lwd=2)
lines(predict(smooth_model_125, x = X_0), col="blue", lwd=2)
lines(predict(smooth_model_auto, x = X_0), col="yellow", lwd=2)

# e)

set.seed(222)
n = length(bmi)

train <- sample(n, 2*n/3, replace = FALSE)
X.train = bmi[train]
y.train = pbfm[train]
X.test = bmi[-train]
y.test = pbfm[-train]
model <- list()
mse <- list()

# Fit simple linear regression model
model$linear <- lm(y.train~X.train, bodyfat)
summary(model$linear)
mse$linear <- mean((y.test - predict(model$linear, x = X.test))^2)

# Fit polynomial regression model
model$poly_3 <- lm(y.train~poly(X.train, degree = 3))
model$poly_5 <- lm(y.train~poly(X.train, degree = 5))
model$poly_8 <- lm(y.train~poly(X.train, degree = 8))
mse$poly_3 <- mean((y.test - predict(model$poly_3, x = X.test))^2)
mse$poly_5 <- mean((y.test - predict(model$poly_5, x = X.test))^2)
mse$poly_8 <- mean((y.test - predict(model$poly_8, x = X.test))^2)
summary(model$poly_3)
summary(model$poly_5)
summary(model$poly_8)

# Fit local regression model (using optimal span found previously)
model$localr = loess(y.train~X.train, span=0.6148132, data= data.frame(X.train))
mse$localr <- mean((y.test - predict(model$localr, x = X.test))^2)
summary(model$localr)

# Fit regression spline models
train_range =range(X.train)
X.seq = seq(from =train_range[1], to= train_range[2])

model$spline_1 = lm(y.train ~ bs(X.train, df=4, degree=1), data=bodyfat )
model$spline_2 = lm(y.train ~ bs(X.train, df=4, degree=2), data=bodyfat )
model$spline_3 = lm(y.train ~ bs(X.train, df=4, degree=3), data=bodyfat )

mse$spline_1 <- mean((y.test - predict(model$spline_1, x = X.test))^2)
mse$spline_2 <- mean((y.test - predict(model$spline_2, x = X.test))^2)
mse$spline_3 <- mean((y.test - predict(model$spline_3, x = X.test))^2)

summary(model$spline_1)
summary(model$spline_2)
summary(model$spline_3)

# compare resulting mses
cbind(mse)

