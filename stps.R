# What is in the package at the moment?
dir("datasummary")

# Add the weather data
use_data(weather, pkg = "datasummary", overwrite = TRUE)

# Add a vignette called "Generating Summaries with Data Summary"
use_vignette("Generating_Summaries_with_Data_Summary", pkg = "datasummary")

# What directories do you now have in your package now?
dir("datasummary")

#' Summary of Numeric Columns
#'
#'
#' @param x A data frame. Non-numeric columns will be removed
#' @param na.rm A logical indicating whether missing values should be removed
#' @import dplyr
#' @import purrr
#' @importFrom tidyr gather
#' @export
data_summary <- function(x, na.rm = TRUE){

    num_data <- select_if(x, .predicate = is.numeric)

    map_df(num_data, .f = numeric_summary, na.rm = na.rm, .id = "ID")
}


#' Data Summary for Numeric Columns
#'
#' Custom summaries of numeric data in a provided data frame
#'
#' @param x A data.frame containing at least one numeric column
#' @param na.rm A logical indicating whether missing values should be removed
#' @import dplyr
#' @import purrr
#' @importFrom tidyr gather
#' @export
#' @examples
#' data_summary(iris)
#' data_summary(airquality, na.rm = FALSE)
data_summary <- function(x, na.rm = TRUE){

    num_data <- select_if(x, .predicate = is.numeric)

    map_df(num_data, .f = numeric_summary, na.rm = na.rm, .id = "ID")

}


#' Data Summary for Numeric Columns
#'
#' Custom summaries of numeric data in a provided data frame
#'
#' @param x A data.frame containing at least one numeric column
#' @param na.rm A logical indicating whether missing values should be removed
#' @import dplyr
#' @import purrr
#' @importFrom tidyr gather
#' @export
#' @examples
#' data_summary(iris)
#' data_summary(airquality, na.rm = FALSE)
#'
## Update the details for the return value
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'  \item ID
#'  \item min
#'  \item median
#'  \item sd
#'  \item max
#' }
#'
#' @export
data_summary <- function(x, na.rm = TRUE){

    num_data <- select_if(x, .predicate = is.numeric)

    map_df(num_data, .f = numeric_summary, na.rm = na.rm, .id = "ID")

}


#' Summary of Numeric Columns
#' Generate specific summaries of numeric columns in a data frame
#'
#' @param x A data frame. Non-numeric columns will be removed
#' @param na.rm A logical indicating whether missing values should be removed
#' @import dplyr
#' @import purrr
#' @importFrom tidyr gather
#' @export
#' @examples
#' data_summary(iris)
#' data_summary(airquality, na.rm = FALSE)
#'
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'  \item ID
#'  \item min
#'  \item median
#'  \item sd
#'  \item max
#' }
#'
## Add in the author of the `data_summary()` function
#' @author My Name <myemail@example.com>
## List the `summary()` function (from the `base` package)
#' @seealso \link[base]{summary}
data_summary <- function(x, na.rm = TRUE){

    num_data <- select_if(x, .predicate = is.numeric)

    map_df(num_data, .f = numeric_summary, na.rm = na.rm, .id = "ID")

}



#' datasummary: Custom Data Summaries
#'
#' Easily generate custom data frame summaries
#'
#' @docType package
#' @name datasummary
"_PACKAGE"


# Generate package documentation
document("datasummary")

# Examine the contents of the man directory
dir("datasummary/man")

# View the documentation for the data_summary function
help("data_summary")

# View the documentation for the weather dataset
help("weather")

#' datasummary: Custom Data Summaries
#'
#' Easily generate custom data frame summaries
#'
#' @docType package
#' @name datasummary
"_PACKAGE"

# Update this function call
utils::globalVariables(c("weather", "Temp"))








