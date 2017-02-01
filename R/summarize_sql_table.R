#' summarize_sql_table
#'
#' I find myself needing to understand what data is in a table, but the table is too large to read into a
#' data frame and then use the DataFrameCleanup function on.
#' To get around this, use this function which does group by's on each column in SQL to get a picture
#' of the table.
#'
#' @param dbConn A connected RODBC odbcDriverConnect object
#' @param tblName The name of the table you want to summarize
#' @keywords SQL, table, find, explore
#' @export
#' @examples
#' # Load packages
#' library(mahPackage)
#' library(RODBC)
#'
#' # Create connection
#' mySqlServerConn <- odbcDriverConnect(connection = "Driver={SQL Server};server=myservername;database=mydbname;trusted_connection=yes")
#'
#' # Call function to summarize table called MyFakeTable
#' myDataFrame <- SqlColumnExplorer(mySqlServerConn, 'MyFakeTable')
#'
#' # Results
#' *********Fill this in
#'

summarize_sql_table <- function(dbConn, tblName) {

  # **************************************************
  # Test Code - delete me
  # **************************************************
  library(RODBC)
  dbConn <- odbcDriverConnect(connection = "Driver={SQL Server};server=<<server>>;database=<<db>>;trusted_connection=yes")
  tblName <- 'mytbl'

  # **************************************************
  # Find all table and column combinations that match
  # **************************************************
  sqlText <- "SELECT t.name as TableName, c.name as ColumnName
              FROM sys.tables t
              INNER JOIN sys.columns c ON t.object_id = c.object_id
              WHERE t.name = '@tblName'"

  sqlStatement <- gsub("@tblName", tblName, sqlText)
  sqlResult <- sqlQuery(dbConn, sqlStatement, as.is=TRUE)
  tblColDF <- sqlResult

  # **************************************************
  # Get the data for each table
  # **************************************************
  sqlText <- "SELECT '@tblName' as TblName, '@colName' as ColName,
              count(*) as num_rows,
              sum(case when @colName = 0 then 1 else 0 end)  / cast(count(*) as decimal) * 100 as pct_zero_rows,
              sum(case when @colName is null then 1 else 0 end) / cast(count(*) as decimal) * 100 as pct_null_rows,
              count(distinct(@colName)) as distinct_values
              FROM db.@tblName"

  allResults <- data.frame()

  for (i in 1:nrow(tblColDF)) {
    # Build the SQL query
    sqlStatement <- gsub("@tblName", tblColDF$TableName[i], sqlText)
    sqlStatement <- gsub("@colName", tblColDF$ColumnName[i], sqlStatement)
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
