#' Cross-Validation subsamples for time series
#'
#' This function takes traints and testts and create a single tibble with both results
#'
#' @param data a dataframe with the time series data that the user wants to iterate.
#' @param cv_size Numeric. Cross-validation window that the user want to test the performance of a model.
#' @param lead_time Numeric. How many periods ahead (lead time) the user wants to predict.
#' @param ... Other arguments associated with traints and testts functions
#'
#' @return a tibble with rows=iterations.
#' \itemize{
#'  \item iteration
#'  \item rest of the columns
#' }
#' @export
#'
#' @examples
#' cvts(rainfall_cr, cv_size=6, lead_time=3, keep_prev=TRUE)
cvts <-function(data, cv_size, lead_time, ...){
    traints(data, cv_size, lead_time) %>%
        left_join(testts(data, cv_size, lead_time, ...), by = "iter")
    }
