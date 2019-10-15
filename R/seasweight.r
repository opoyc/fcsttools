#' Seasonal weights estimation by robust regression
#'
#' This function fits a State Space Robust Regression to estimate seasonal weights from the monthly
#' marginal effects.
#'
#' The response variable is scaled with mean zero and standard deviation 1. In the first iteration
#' 12 observations are used to estimate the weights, sequentially a observation is included and the
#' marginal effects are recalculated through following a State Space method. To complete the weights for T-12 months a backcasting
#' estimate is performed using Cubic Smoothing Spline method.
#' @param data a dataframe
#' @param value depended variable
#' @param date a date column of montly data
#'
#' @return a tibble with the monthly weights
#' @export
#' @import purrr dplyr feasts forecast ggeffects tidyr lubridate
#' @importFrom MASS rlm
#' @importFrom tsibble yearmonth
#' @examples
#' data
utils::globalVariables(c("gaps", "obs", "duplicate_date", "signal", "season_year", "month_name", "weight_ds", "weight", "."))
seasweight <- function(data, value, date){
    date <- enquo(date)
    value <- enquo(date)
    suppressWarnings(
        {
            weights_1 <- data %>%
                mutate(date=tsibble::yearmonth(!!date)
                       , signal=scale(!!value)[,1]) %>%
                dplyr::select(date, signal) %>%
                as_tsibble(index=date) %>%
                STL(signal~season(window=12)) %>%
                as_tibble() %>%
                transmute(date=as.Date(date)
                          , month_name=lubridate::month(date, label=T)
                          , signal=remainder+season_year) %>%
                mutate(weight_ds=tsibble::stretch2(.x = signal, .y=month_name
                                                   , .f = ~MASS::rlm(.x~.y) %>%
                                                       ggeffects::ggpredict() %>%
                                                       .$.y %>%
                                                       as_tibble() %>%
                                                       transmute(month_name=month.abb[x]
                                                                 , sig=ifelse(0>conf.low & 0<conf.high, F, T)
                                                                 , weight=ifelse(sig==T, predicted, 0)) %>%
                                                       dplyr::select(-sig)
                                                   , .init=12, .step = 1)) %>%
                filter(!is.na(weight_ds)) %>%
                dplyr::select(date, weight_ds) %>%
                unnest(weight_ds) %>%
                spread(month_name, weight) %>%
                dplyr::select(date, month.abb)
            weights_2 <- map(weights_1 %>%
                                 arrange(desc(date)) %>%
                                 .[,-1], ~splinef(.x, h = 11)$mean %>% as_tibble) %>%
                bind_cols() %>%
                set_names(nm = month.abb) %>%
                mutate(date=min(weights_1$date) %m-% months(rownames(.) %>% as.numeric())) %>%
                dplyr::select(date, month.abb)

            bind_rows(weights_1, weights_2) %>%
                arrange(date)
        }
    )}
