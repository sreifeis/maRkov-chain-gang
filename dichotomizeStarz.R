#' Dichotomize Starz
#'
#' Turns the multinomial ratings into a binomial rating
#'
#' Rating values can ONLY be 1,2,3,4,5. Values of 1,2,3 are 
#' given a new rating of 0, values or 4,5 are given a new 
#' rating of 1
#'
#' 
#' @param star_vector a vector of integers valued between 1 and 5
#' @return a vector of 0's and 1's; the dichomomized rating
#' 
#' @export 
dichotomizeStarz <- function(star_vector)
{
  if(sum(floor(star_vector) != ceiling(star_vector))!= 0)
  {
    stop("Ratings must be an integer value between 1 and 5")
  }
  
  not_na_obs <- which(!is.na(star_vector))
  
  if((sum(star_vector[not_na_obs] < 1 | star_vector[not_na_obs] > 5) > 0)){
    stop("Ratings can only be 1,2,3,4,5")
  }
    
  if(length(not_na_obs)!=length(star_vector))
  {
    warning("There are some missing values in your vector")
  }
  
  #make the vector missing so as not to accidentally assign values
  rate_vector <- rep(NA,length(star_vector))

  high_val  <- which(4 <= star_vector & star_vector <= 5)
  low_val <- which(1 <= star_vector & star_vector <= 3)
  
  rate_vector[high_val] <- 1
  rate_vector[low_val] <- 0
  return(rate_vector)
}
