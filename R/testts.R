#' Cross-validated time series test set
#'
#' Creates a cross-validated test set for a given number of time series iterations.
#'
#' This function follows a tidy approach to create test sets applicable to time series data.
#' It admits tsibble, tibble and data frame objects.
#' @param data a dataframe with the time series data that the user wants to iterate.
#' @param cv_size Numeric. Cross-validation window that the user want to test the performance of a model.
#' @param lead_time Numeric. How many periods ahead (lead time) the user wants to predict.
#' @param keep_prev Logical. Controls if the test set includes previous data when \eqn{lead_time>2}
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
#' testts(rainfall, cv_size=6, lead_time=3, keep_prev=TRUE)
testts <- function(data, cv_size, lead_time, keep_prev=FALSE){
    test <- data[(nrow(data)-cv_size-lead_time+2):nrow(data),]
    iter <- list()
    count <- vector()
    if(keep_prev==F){
        for(i in 1:(nrow(test))){
            iter[[i]] <- test[(i+lead_time-1),]
            }
        } else {
            for(i in 1:(nrow(test))){
                iter[[i]] <- test[i:(i+lead_time-1),]
                }
            }
    test <- iter %>% enframe(name="iter", value="test") %>% .[1:cv_size,]
    return(test)
}
