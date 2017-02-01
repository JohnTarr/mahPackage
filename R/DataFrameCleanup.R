#' DataFrameSorter
#'
#' After reading in a large data frame, I found myself having to look for columns that were entirely empty or zero.
#' Also, columns which all have the same value are of interest, as they won't make much difference in any analysis.
#' This function speeds that up by re-ordering the columns so you can quickly see what you have.
#' @param myDataFrame Any data frame
#' @keywords dataframe, sort, clean
#' @export
#' @examples
#' # Load packages
#' library(mahPackage)
#'
#' # Call function
#' myDataFrame <- DataFrameSorter(myDataFrame)
#'
#' # Results - fix these later
#'     TblName           ColName           Value  Count
#' 1   ProductMaster     targetMarket      NW     2000
#' 2   ProductMaster     targetMarket      SE     1000
#' 3   ProductMaster     targetMarket      ...    ...
#' 4   MktStudies        marketStudyYear   2015   500
#' 5   MktStudies        marketStudyYear   2014   600
#' 6   MktStudies        marketStudyYear   ...    ...
#' 6   ...               ...               ...    ...

DataFrameSorter <- function(myDataFrame)
{

  # **********************************************
  # Create connection to SQL Server
  # **********************************************

  # Put all columns with the same data at the end
  namesList <- names(myDataFrame[sapply(myDataFrame, function(x) length(unique(x)) == 1)])
  myDataFrame$SameValue <- 'AllTheSameValue >>'
  myDataFrame <- myDataFrame[c(setdiff(names(myDataFrame), namesList), namesList)]

  # Move the "empty" columns to the end
  myDataFrame$EmptyFields <- 'EmptyFields >>'

  # Get the names of the numeric columns with all 0's
  numericNames <- myDataFrame[sapply(names(myDataFrame), function(col) is.numeric(myDataFrame[,col]))]
  zeroNames <- names(numericNames[sapply(numericNames, function(x) all(x == 0))])

  # Get the names of the character columns that are blank
  charNames <- myDataFrame[sapply(names(myDataFrame), function(col) is.character(myDataFrame[,col]))]
  blankNames <- names(numericNames[sapply(numericNames, function(x) all(x == ''))])

  # Move them to the end
  myDataFrame <- myDataFrame[c(setdiff(names(myDataFrame), zeroNames), zeroNames)]
  myDataFrame <- myDataFrame[c(setdiff(names(myDataFrame), blankNames), blankNames)]

  # Return value
  myDataFrame



  # Get rid of the columns with below conditions
  # myDataFrame <- myDataFrame[!sapply(myDataFrame, function(x) all(x == 0))]
  # myDataFrame <- myDataFrame[!sapply(myDataFrame, function(x) all(x == ".0000"))]
  # myDataFrame <- myDataFrame[!sapply(myDataFrame, function(x) all(as.character(x) == '1899-11-30 00:00:00.000'))]
  # myDataFrame <- myDataFrame[!sapply(myDataFrame, function(x) all(trimws(x) == ''))]



}
