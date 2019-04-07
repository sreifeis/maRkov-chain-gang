##############################################################################
## Project: build_staRz.R
## Script purpose: Creates staRz R pkg & runs tests/checks on pkg
## Date Created: Apr 3, 2019
## Author: Sarah Reifeis
## Last Modified: Apr 3, 2019
## Modifications: 
## Notes : 
##############################################################################

library(devtools)
library(pkgbuild)
library(roxygen2)
library(testthat)
#library(root)

## Creating
create("staRz", rstudio=FALSE)
#usethis::create_package("staRz", rstudio=FALSE)

## Building & documenting
document(pkg="staRz")
build("staRz")

## Testing
document(pkg="staRz")
load_all("staRz")


## Checking
setwd("~/SP19/BIOS 735/maRkov-chain-gang/staRz/tests")
check(manual=TRUE) # error: package can't be installed

