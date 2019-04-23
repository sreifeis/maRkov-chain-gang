######################################
## Cleans raw merged data and exports 
##  test and training dsets as one
######################################

library(usethis)
library(readr)

dat <- read_csv("merged.csv")

##########################
## Read in and set up data
##########################

# Complete cases only
dat <- dat[ complete.cases(dat), ]

# Subset variables
dat2 <- dat[,-c(1,3,9,31,33,36,38)]

# Transform LDA probability variables
LDA.vars <- 2:26
dat3 <- dat2
dat3[,LDA.vars] <- log( dat2[,LDA.vars] / (1 - dat2[,LDA.vars]) )

# Create binary response variable
dat3$stars_binary <- ifelse( dat3$reviews.rating >= 4, 1, 0 )

#######################################################
## Define y and X in our data
## y = nx1 vector, X = nxp matrix with intercept column
#######################################################

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
## Combine X.train, y.train, X.test, and y.test to one dset
#############################################################
all.train <- cbind(X.train, y.train)
all.test <- cbind(X.test, y.test)

save(all.train, file = "data/all.train.Rdata")
save(all.test, file = "data/all.test.Rdata")

