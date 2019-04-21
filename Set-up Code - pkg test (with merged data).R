## HIL: code copied and slightly modified from 'Set-up Code - Runs Nate Lasso - pkg.R'

# Read in merged data
setwd("/Users/nathanbean/Documents/Spring 2019/BIOS 735/Final Project")
dat <- read.csv("merged.csv")
dat <- dat[ complete.cases(dat), ]

# Subset variables
dat2 <- dat[,-c(1,3,9,31,33,36,38)]

# Transform LDA probability variables
LDA.vars <- 2:26
dat3 <- dat2
dat3[,LDA.vars] <- log( dat2[,LDA.vars] / (1 - dat2[,LDA.vars]) )

# Create binary response variable
dat3$stars_binary <- ifelse( dat3$reviews.rating >= 4, 1, 0 )



######################################################
# Define y and X in our data
# y = nx1 vector, X = nxp matrix with intercept column
######################################################

y <- as.matrix( dat3[,33] )
X_no_scale <- as.matrix( dat3[,-c(1,33)] )
# X <- scale(X_no_scale, center = T, scale = T)
n <- dim(X_no_scale)[1]

###################################################
## Subset X into training, test, and holdout data
## 50% training, 50% test
###################################################

all.samples = 1:n
num.test = round(n/2, digits = 0)

set.seed(735)
test.rows = sample(all.samples, size = num.test, replace = F)

X.train0 = cbind(1, X_no_scale[-test.rows,])
y.train = y[-test.rows]
X.test0 = cbind(1, X_no_scale[test.rows,])
y.test = y[test.rows]

#############################################################
## Standardize training and test sets using training values
#############################################################

train.means <- colMeans(X.train0)
train.sds <- apply(X.train0, 2, sd)
X.train <- X.train0
X.test <- X.test0

# Only scale covariates that are not intercept
for(j in 2:dim(X.train0)[2]){
  X.train[,j] <- (X.train0[,j] - train.means[j]) / train.sds[j]
  X.test[,j] <- (X.test0[,j] - train.means[j]) / train.sds[j]
}


#############################################################
## Find maximum possible lambda. At this value, B vector = 0
#############################################################

library(stats) # For 'glm'
# Update "staRz_0.0.1.tar.gz" using "Tools" every time package is updated
library(staRz)
fit = glm(y.train ~ 1, family = binomial)

# Extract intercept
int = fit$coefficients
B_test = c(int, numeric((ncol(X.train) - 1)))

prob = exp(int) / (1 + exp(int))
prob.vec <- rep(prob, num.train)  # SAR: I think this line is not used

# Find lambda_max
W = prob*(1-prob)  # Weight matrix when B = 0 (other than intercept)
# Working response for intercept-only results
y_wr = (y.train - prob) / (prob * (1-prob)) + X.train %*% B_test 

lam_option = numeric(ncol(X.train))
for(j in 1:ncol(X.train)){
  lam_option[j] = abs( (1/nrow(X.train)) * sum( X.train[,j] * W * (y_wr - X.train[,-j] %*% B_test[-j]) ) )
}

lambda_max = max(lam_option)
epsilon = 0.01
lambda_min = lambda_max * epsilon

# Recommendation by 761 notes: perform sequence on log scale
log_lam = seq(from = log(lambda_max), to = log(lambda_min), by = -0.05)
# Re-transform back to regular scale
lambda = exp(log_lam)

############################################
## Run LQA.lasso function from staRz
############################################

## Load staRz pkg
library(staRz)

## Run Lasso using R wrapper function from staRz

# Update data matrices
X.train <- as.matrix(X.train)
# Specify initial B = 0 (due to first lambda = maximum lambda)
B.pkg = numeric(length = ncol(X.train))
# Initialize list that will store the B vectors for each lambda
results.pkg = list()
# For lambda max, B = 0
results.pkg[[1]] = B.pkg
# Initialize vector to store BIC values
bic.pkg = numeric(length(lambda))
bic.pkg[1] = 10^10 # Some arbitrarily large value

# Loop through all lambda values in Lasso function - see scratch work below for running only one iteration
time.start <- Sys.time()
for(l in 2:length(lambda)){
  # Calculate new B for specified lambda
  results.pkg[[l]] = staRz::LQA.lasso(X.train, Y=y.train, B.pkg, lambda[l], tol = 10^-4, max_it = 1000)
  # Update B with new value
  B.pkg = results.pkg[[l]]
  # Calculate EBIC - Extended BIC (Chen and Chen 2008)
  # Calculate linear predictor with updated B
  eta.pkg = X.train %*% B.pkg
  # EBIC = -2 log-lik + (num params) * log(num observations) + 2 * (num params) * constant * log(num total param - cols of X)
  bic.pkg[l] = -2 * sum( y.train * eta.pkg - log(1 + exp(eta.pkg)) ) +
    sum(B.pkg!=0) * log(nrow(X.train)) + 2 * sum(B.pkg!=0) * 0.5 * log(ncol(X.train))
  # Potential options for constant in last term: 0.25, 0.5, 1
  # According to Chen and Chen paper on extended BIC (EBIC)
  # 0.5 good option
}
time.stop <- Sys.time()
time.stop - time.start

## Results
which.min(bic.pkg[-1]) # smallest BIC
beta.coefs <- results.pkg[[which.min(bic.pkg[-1])]]

########################################################################
## Calculate probability of success for observations in test set
########################################################################

test.probs <- exp( X.test %*% beta.coefs ) / ( 1 + exp( X.test %*% beta.coefs ) )
hist(test.probs)
table(y.test)


########################################################################
## Function to create confustion matrix
########################################################################
conf.mat.fun <- function( y.vals, prob.vals, cutoff ){
  # Predict Y for given cutoff
  y.pred <- ifelse( prob.vals >= cutoff, 1, 0 )
  
  # Calculate number of true positives, true negatives, false positives, false negatives
  num.tp <- sum( ifelse( y.vals == 1 & y.pred == 1, 1, 0 ) )
  num.tn <- sum( ifelse( y.vals == 0 & y.pred == 0, 1, 0 ) )
  num.fp <- sum( ifelse( y.vals == 0 & y.pred == 1, 1, 0 ) )
  num.fn <- sum( ifelse( y.vals == 1 & y.pred == 0, 1, 0 ) )
  
  # Output confustion matrix (observed on rows, predicted on columns)
  #     1   0
  # 1   tp  fn
  # 0   fp  tn
  conf.mat <- matrix(0, nrow=2, ncol=2)
  conf.mat[1,1] <- num.tp
  conf.mat[1,2] <- num.fn
  conf.mat[2,1] <- num.fp
  conf.mat[2,2] <- num.tn
  
  return(conf.mat)
}

# Mis-classification error rate for test set using cutoff of .5
test.conf.mat <- conf.mat.fun( y.test, test.probs, .5 )
test.conf.mat
mcer.test <- (test.conf.mat[1,2] + test.conf.mat[2,1]) / length(y.test)
mcer.test

