#' readExcelCells
#'
#' This is a wrapper around the XLSX package to save multiple data frames in their own tab.
#' @param dirToScan A path name to scan. Subfolders will be scanned
#' @param whatToRead A data frame with two columns $position and $name specifying the column name to use and the position of the cell to read
#' #' @param tabsToIgnore A vector of tab names. The function will ignore these tabs. Cannot be set with tabsToRead
#' #' @param tabsToRead A vector of tab names. The function will only read from these tabs. Cannot be set with tabsToIgnore
#' #' @param extensionsToScan A vector of file extensions. All other extensions will be ignored.
#' @keywords dataframe, xlsx
#' @export
#' @examples
#' # Load packages
#' library(mahPackage)
#' 
#' # Set variables
#' ignoreThese = c('Sheet1', 'etc')
#'
#' # Call function
#' readExcelCells("/SourceData/readThis", tabsToIgnore = ignoreThese)


readExcelCells <- function(dirToScan, whatToRead, tabsToIgnore = NULL, tabsToRead = NULL, 
                        extensionsToScan = c('xlsx', 'xls', 'xlsm')){
  # Error checking
  stopifnot(!(!is.null(tabsToRead) & !is.null(tabsToIgnore)))
  
  # Turn off strings as factors for the data reads
  options(stringsAsFactors = FALSE)
  
  # Read in file names and get file extensions
  filesToScan <- data.frame(relativeFileName = dir(dirToScan, all.files = FALSE, full.names = TRUE, recursive = TRUE))
  filesToScan$fileExtension <- regmatches(filesToScan$relativeFileName, 
                                           regexpr('[[:alnum:]]+$', filesToScan$relativeFileName))
  filesToScan$fileName <- basename(filesToScan$relativeFileName)
  
  # Eliminate any invalid file types
  filesToScan <- filesToScan[filesToScan$fileExtension %in% extensionsToScan,]
  filesToScan <- filesToScan[!(substr(filesToScan$fileName, 1, 2) == '~$'),]
  
  # Initialize prior to loop
  myDataFrame <- data.frame()
  myIndex <- 0

  for(i in 1:nrow(filesToScan)){
    myWB <- loadWorkbook(filesToScan$relativeFileName[i])
    mySheets <- getSheets(myWB)
    if(!is.null(tabsToIgnore)){
      mySheets <- mySheets[!(mySheets %in% tabsToIgnore)] # remove tabsToIgnore
    }
    else if(!is.null(tabsToRead)){
      mySheets <- mySheets[mySheets %in% tabsToRead] # remove everything but tabsToRead
    }
      
    for(j in 1:length(mySheets)){
      myIndex <- myIndex + 1
      baseData <- data.frame(relativeFileName = filesToScan$relativeFileName[i], 
                             workbook = filesToScan$fileName[i], 
                             worksheet = mySheets[j])    
      dynamicData <- data.frame(sapply(whatToRead$position, 
                                       function(x) readWorksheet(myWB, mySheets[j], region = x, 
                                                                 header=FALSE, autofitRow=FALSE, autofitCol=FALSE)))
      names(dynamicData) <- whatToRead$name
      fullData <- cbind(baseData, dynamicData)
      myDataFrame <- rbind(myDataFrame, fullData)
    }
  }
  # Return value
  myDataFrame
}



