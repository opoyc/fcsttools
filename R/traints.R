#' Cross-validated time series train set
#'
#' Creates a cross-validated train set for a given number of time series iterations.
#'
#' This function follows a tidy approach to create train sets applicable to time series data.
#' It admits tsibble, tibble and data frame objects.
#' @param data a dataframe with the time series data that the user wants to iterate.
#' @param cv_size Numeric. Cross-validation window that the user want to test the performance of a model.
#' @param lead_time Numeric. How many periods ahead (lead time) the user wants to predict.
#'
#' @return a tibble with rows=iterations.
#' \itemize{
#'  \item iteration
#'  \item rest of the columns
#' }
#' @export
#' @import tibble
#' @import dplyr
#'
#' @examples
#' traints(rainfall_cr, cv_size=6, lead_time=3)
traints <- function(data, cv_size, lead_time){
    train <- data[1:(nrow(data)-cv_size-lead_time+1),]
    iter <- list(train)
    count <- vector()
    for(i in 1:(cv_size-1)){
        count[i] <- sum(i)
        iter[[i+1]] <- data[1:(nrow(train)+max(count)),]
    }
    train <- iter %>% enframe(name = "iter", value = "train")
    return(train)
}
