# MANDATORY EXERCISE 1 (Spring 2020)
# By Ida Austad 

# PROBLEM 1

# read in, inspect and prep data
path <- file.path(getwd() , "res_bodyfat.csv")
bodyfat <- read.csv(path)
summary(bodyfat)
head(bodyfat)
attach(bodyfat)

#a)
plot(bmi, pbfm, main="Linear Gaussian Model")
fit_linear <- lm(pbfm ~ bmi)
print(summary(fit_linear))
x = (seq(min(bmi), max(bmi), length = 200))
beta = coef(fit_linear)
lines(x, beta[1] + beta[2]* x, col = 2, lty = 1, lwd = 2)

#get diagnostics plots (one at a time)
plot(fit_linear)

#b) 

# fit model with log transform of exp.variable
fit_log <- lm(pbfm ~ log(bmi))
print(summary(fit_log))

# plot model
beta = coef(fit_log)
x = log(seq(min(bmi), max(bmi), length = 200))
plot(log(bmi), pbfm, main="Linear Model with logarithmic transform")
lines(x,(beta[1] + beta[2]*x), col = 2) 

#plot diagnostics
par(mfrow=c(2,2))
plot(fit_log)

# fit model with both linear and quadratic effect of exp. var
fit_quad<- lm(pbfm ~ bmi + I(bmi^2))
print(summary(fit_quad))

#plot model
beta = coef(fit_quad)
x = (seq(min(bmi), max(bmi), length = 200))
par(mfrow=c(1,1))
plot(bmi, pbfm, main="Quadratic model")
lines(x, beta[1] + beta[2]*x + beta[3]*x^2, col=2)

#plot diagnostics
par(mfrow=c(2,2))
plot(fit_quad)

#c) find order of polynomial which best fits the data (using cv.glm method from boot library - ref 5.5.3 in ILS)
#create list to hold errors of each fold
set.seed(1)
cv_errors = rep(0,10)
for (i in 1:10){
  glm_fit = glm(pbfm ~ poly(bmi,i), data=bodyfat)
  cv_errors[i] = cv.glm(bodyfat, glm_fit, K=10)$delta[1]
}
#print cv errors
cv_errors

#get index of smallest value and error
which.min(cv_errors)
min(cv_errors)

#plot best model (order 4)
par(mfrow=c(1,1))
plot(bmi, pbfm, main="Generalized linear model of order 4")
fit_best <- glm(pbfm ~ poly(bmi,4), data=bodyfat)
print(summary(fit_best))
x = (seq(min(bmi), max(bmi), length = 200))
y = predict(fit_best, list(bmi = x), type="response")
lines(x, y, col = 2, lty = 1, lwd = 2)

#plot diagnostics
par(mfrow=c(2,2))
plot(fit_best)

#d) using AIC
set.seed(1)
cv_AIC = rep(0,10)
for (i in 1:10){
  glm_fit = glm(pbfm ~ poly(bmi,i), data=bodyfat)
  cv_AIC[i] = AIC(glm_fit)
}
#print cv errors
cv_AIC

#get index of smallest value and error
which.min(cv_AIC)
min(cv_AIC)

#-----------------------------------------------------------

# PROBLEM 2

# read in, inspect and prep data
path <- file.path(getwd() , "oral_ca.csv")
oral <- read.csv(path)
summary(oral)
head(oral)

#create array saying false/true (non-smoker / smoker)
smoker_nonSmoker = cigs == 0

# a)
# create new column indicating smoker vs non-smoker
oral$smoker <- with(oral, ifelse(cigs==0, "non-smoker", "smoker"))
attach(oral)
smoker_table <- table(ccstatus, smoker, dnn = c('ccstatus', 'smoker'))

# calulation of binomial mean
mean_common = (smoker_table["1", "non-smoker"] + smoker_table["1", "smoker"] ) / nrow(oral)
mean_smoker =  smoker_table["1", "smoker"] / (smoker_table["0", "smoker"] + smoker_table["1", "smoker"] ) 
mean_non_smoker = smoker_table["1", "non-smoker"] / (smoker_table["0", "non-smoker"] + smoker_table["1", "non-smoker"] )

# calculation of binomial vairance: Var(X) = n*p*(1-p)
sd_common = sqrt(mean_common * (1 - mean_common) / nrow(oral))
sd_smoker = sqrt(mean_smoker * (1 - mean_smoker) / (smoker_table["0", "smoker"] + smoker_table["1", "smoker"]))
sd_non_smoker = sqrt(mean_non_smoker * (1 - mean_non_smoker) / (smoker_table["0", "non-smoker"] + smoker_table["1", "non-smoker"] ))

# b)
# compute the likelihood ratio statistics test
llik_1 = sum(dbinom(smoker_nonSmoker[ smoker_nonSmoker == TRUE], 1, mean_smoker, log = TRUE)) + 
  sum(dbinom(smoker_nonSmoker[ smoker_nonSmoker == FALSE], 1, mean_non_smoker, log = TRUE))
llik_pi_2 = sum(dbinom(smoker_nonSmoker, 1, mean_common, log=TRUE))

w = 2 * (llik_1 - llik_2)
p.val = 1 - pchisq(w, df = 1)
p.val #answer shows zero

# c)
# create x and y columns
y = oral$ccstatus == 1
x = oral$cigs > 0

#fit binomial model and print results
bin_mod = glm(y ~ x, family = 'binomial')
print(summary(bin_mod))

# get coefficients
beta = bin_mod$coefficients
print(beta)

pi_nonSmoker = exp(beta[1]) / (1 + exp(beta[1]))
pi_smoker = exp(beta[1] + beta[2]) / (1 + exp(beta[1] + beta[2]))

#calculate odds and log odds ratio
odds_nonSmoker = mean_non_smoker / (1 - mean_non_smoker)
odds_smoker = mean_smoker / (1 - mean_smoker)

log_odds_ratio = (odds_smoker / odds_nonSmoker)
log_odds_ratio

# d) 
# create the x and y variables, but with a continuous x
x = cigs
y = ccstatus
continuous_mod = glm(y ~ x, family = 'binomial')
print(summary(continuous_mod))

#e)
#include all features in the model
total_mod = glm(y ~  oral$drinks + oral$cigs + oral$age + oral$sex, family = 'binomial')
print(summary(total_mod))

#f) 
#dropping age
no_age_mod = glm(y ~  oral$drinks + oral$cigs + oral$sex, family = 'binomial')
print(summary(no_age_mod))

#g) 
poly_drinks_mod = glm(y ~  poly(oral$drinks,2) + oral$cigs + oral$sex, family = 'binomial')
print(summary(poly_drinks_mod))

#h)
poly_cigs_mod = glm(y ~  oral$drinks + poly(oral$cigs,2) + oral$sex, family = 'binomial')
print(summary(poly_cigs_mod))
