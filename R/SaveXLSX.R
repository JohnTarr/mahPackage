#' saveXLSX
#'
#' This is a wrapper around the XLSX package to save multiple data frames in their own tab.
#' @param file A file name for the xlsx file
#' @param ... One or more data frames
#' @keywords dataframe, xlsx
#' @export
#' @examples
#' # Load packages
#' library(mahPackage)
#'
#' # Call function
#' saveXLSX("myFileName.xlsx", dataFrame1, dataFrame2)

saveXLSX <- function (file, ...)
{
  file <- paste(file, '.xlsx', sep='')
  
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  myWorkbook <- xlsx::createWorkbook()
  colNameStyle <- xlsx::CellStyle(myWorkbook) + xlsx::Font(myWorkbook, isBold=TRUE) + xlsx::Border() # header
  
  for (i in 1:nobjects) {
    # Convert posix to character for Excel
    posixNames <- names(objects[[i]][sapply(names(objects[[i]]), function(col)  inherits(objects[[i]][,col],'POSIXct'))])
    objects[[i]][,posixNames] <- as.character(objects[[i]][,posixNames])
    
    mySheet <- xlsx::createSheet(myWorkbook, objnames[i])
    xlsx::addDataFrame(as.data.frame(objects[[i]]), mySheet, colnamesStyle = colNameStyle, row.names = FALSE)
    for(j in 1:ncol(objects[[i]])) {
      xlsx::autoSizeColumn(mySheet, j)
    }
    xlsx::createFreezePane(mySheet, 2, 1) # Freeze the top row
  }
  xlsx::saveWorkbook(myWorkbook, file)
  #print(paste("Workbook", file, "has", nobjects, "worksheets."))
}


