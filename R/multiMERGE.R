#' multiMerge
#'
#' This function lets you merge multiple data frames at once
#' @param allValues Logical, if TRUE, all values included, if false, only matches
#' @param ... One or more data frames
#' @keywords dataframe, merge, multi
#' @export
#' @examples
#' # Load packages
#' library(mahPackage)
#'
#' # Call function
#' multiMERGE(TRUE, dataFrame1, dataFrame2, dataFrame3)
#'
#' # expand on this

multiMERGE <- function (allValues, ...)
{
  Reduce(function(x, y) merge(x, y, all=allValues),
         list(...))
}

# Code for testing
#
#
# dataFrame1 <- data.frame(id=c(1,2,3,4), val1=c('a','b','c','d'))
# dataFrame2 <- data.frame(id=c(1,2,3,4), val2=c('aa','bb','cc','dd'))
# dataFrame3 <- data.frame(id=c(1,2,3), val3=c('aaa','bbb','ccc'))
#
# allDataFrames <- multiMERGE(T, dataFrame1, dataFrame2, dataFrame3)





