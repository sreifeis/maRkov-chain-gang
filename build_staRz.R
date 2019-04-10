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

## Creating:
#create("staRz", rstudio=FALSE)
#devtools::use_rcpp()

## Building/Updating:
# Instead of load_all(), Click "Build"->"Install and Restart"
# ^^ Do this after I add anything to the pkg, and then try 
#    running the added function to check that it works

## Testing:
devtools::test()
# Instead, could also click "Build"->"Test Package"
# Modify any tests that don't pass, and re-do line above until all tests pass

## Checking:
setwd("~/SP19/BIOS 735/maRkov-chain-gang/staRz/tests")
check(manual=TRUE) # error: package can't be installed

