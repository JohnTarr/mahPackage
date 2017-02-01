#' SQL Column Explorer
#'
#' Uses sys.columns to find all columns that match your search, and the tables where those columns exist.
#' Then builds a data frame of the counts of all values for those column and table combinations.
#' Useful when you are looking for data in an unfamiliar or large database.
#' @param dbConn A connected RODBC odbcDriverConnect object
#' @param colSearch Part of the column name you want to search for
#' @keywords SQL, column, find, explore
#' @export
#' @examples
#' # Load packages
#' library(mahPackage)
#' library(RODBC)
#'
#' # Create connection
#' mySqlServerConn <- odbcDriverConnect(connection = "Driver={SQL Server};server=myservername;database=mydbname;trusted_connection=yes")
#'
#' # Call function looking for all columns containing "market"
#' myDataFrame <- SqlColumnExplorer(mySqlServerConn, '\'%market%\'')
#'
#' # Results
#'     TblName           ColName           Value  Count
#' 1   ProductMaster     targetMarket      NW     2000
#' 2   ProductMaster     targetMarket      SE     1000
#' 3   ProductMaster     targetMarket      ...    ...
#' 4   MktStudies        marketStudyYear   2015   500
#' 5   MktStudies        marketStudyYear   2014   600
#' 6   MktStudies        marketStudyYear   ...    ...
#' 6   ...               ...               ...    ...


SqlColumnExplorer <- function(dbConn, colSearch) {

  # **************************************************
  # Find all table and column combinations that match
  # **************************************************
  sqlText <- "SELECT t.name as TableName, c.name as ColumnName
              FROM sys.tables t
              INNER JOIN sys.columns c ON t.object_id = c.object_id
              WHERE c.name LIKE <<colsearch>>
              ORDER BY t.name, c.name"

  sqlStatement <- gsub("<<colsearch>>", colSearch, sqlText)
  sqlResult <- sqlQuery(dbConn, sqlStatement, as.is=TRUE)
  tblColDF <- sqlResult

  # **************************************************
  # Get the data for each table
  # **************************************************
  sqlText <- "SELECT \'<<tblname>>\' as TblName, \'<<colname>>\' as ColName, <<colname>> as Value, count(<<colname>>) as Count
              FROM db.<<tblname>>
              GROUP BY <<colname>>"

  allResults <- data.frame()

  for (i in 1:nrow(tblColDF)) {
    # Build the SQL query
    sqlStatement <- gsub("<<tblname>>", tblColDF$TableName[i], sqlText)
    sqlStatement <- gsub("<<colname>>", tblColDF$ColumnName[i], sqlStatement)
    # Execute SQL
    sqlResult <- sqlQuery(SQLServer, sqlStatement, as.is=TRUE, errors=TRUE)
    # Add column to data frame
    allResults <- rbind(allResults, sqlResult)
  }

  # **************************************************
  # Return the result as a data frame
  # **************************************************
  as.data.frame(allResults)
}















