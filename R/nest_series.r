#' Clean and nest grouped series
#'
#' Executes a set of sanity checks to prepare the analysis of monthly data
#' @param data a dataframe consisting in hierarchical series
#' @param key name of the column (unquoted) that will be used as a key
#' @param value column name of the univariate time series value
#' @param date column name of the date
#'
#' @return a nested series grouped by key
#' \itemize{
#'  \item cleaned series
#' }
#' @import purrr
#' @import dplyr
#' @export
#' @examples
#' nest_series(data=rainfall, key=country, value=rainfall_mm, date=date)
nest_series <- function(data, key, value, date){
    key <- enquo(key)
    value <- enquo(value)
    date <- enquo(date)
        frame1 <- data %>%
            dplyr::select(!!key, !!value, !!date) %>%
            set_names(nm = c("key", "value", "date")) %>%
            group_nest(key) %>%
            mutate(gaps=map_dbl(data, ~sum(with(.x, date[-1]-date[-nrow(.x)])>31))
                   , obs=map_dbl(data, ~nrow(.x))
                   , duplicate_date=map_lgl(data, ~!length(unique(.x$date))==length(.x$date)))
        message(sum(frame1$obs>24), " nested series have more than 24 observations. Passing.")
        message(sum(frame1$gaps>0), " nested series have date gaps, please clean them. Passing the rest.")
        message(sum(frame1$duplicate_date>0), " nested series have duplicate dates. Passing the rest.")
        if(sum(frame1$duplicate_date)==nrow(frame1)){
            message("All nested series have duplicate dates, are you combining raw with cleansed? Filter first")
        }
        return(
        frame1 %>%
            filter(gaps==0, obs>24, duplicate_date==F) %>%
            dplyr::select(key, data))
}
