#' Read Input Data
#'
#' Reads input data from a source file. Currently supports RDS format only.
#'
#' @param sourceFile Character string specifying the path to the input file
#' @return The data object read from the RDS file
#' @export
#' @examples
#' \dontrun{
#' data <- readInput("input_data.rds")
#' }
readInput <- function(sourceFile) {
    # for now every input-type can be read via RDS
    return(readRdsInput(sourceFile))
}

#' Store Result Data
#'
#' Stores a result object to an output file in RDS format.
#'
#' @param result The R object to be stored
#' @param outputFile Character string specifying the path to the output file
#' @return The return value from the underlying RDS storage function
#' @export
#' @examples
#' \dontrun{
#' result <- list(data = c(1, 2, 3))
#' storeResult(result, "output.rds")
#' }
storeResult <- function(result, outputFile) {
    return(storeRdsOutput(result, outputFile))
}

#' Store Result to Text File
#'
#' Writes a result object to a text file. Only writes if both outputFile and
#' result are non-null and non-empty.
#'
#' @param result The R object to be written to file (will be converted to text)
#' @param outputFile Character string specifying the path to the output file.
#'   If NULL or empty string, no file is written
#' @return NULL (invisible). Function is called for its side effect of writing to file
#' @export
#' @examples
#' \dontrun{
#' result <- "Hello World"
#' storeToFile(result, "output.txt")
#' }
storeToFile <- function(result, outputFile) {
    if(!is.null(outputFile) && outputFile != "" && !is.null(result)) {
        logger.debug("Writing to file %s", outputFile)
        write(paste(result), file = outputFile)
    } else {
        logger.debug("Skip writing to file: no output File or result is missing")
    }
}

#' Get Source File Path
#'
#' Retrieves the source file path from the SOURCE_FILE environment variable.
#'
#' @return Character string containing the source file path, or empty string if
#'   the environment variable is not set
#' @keywords internal
#' @examples
#' \dontrun{
#' # Assuming SOURCE_FILE environment variable is set
#' source_path <- sourceFile()
#' }
sourceFile <- function() {
    result <- Sys.getenv(x = "SOURCE_FILE", "")
    logger.debug("sourceFile: %s", result)
    result
}

#' Get Output File Path
#'
#' Retrieves the output file path from the OUTPUT_FILE environment variable.
#'
#' @return Character string containing the output file path, or empty string if
#'   the environment variable is not set
#' @keywords internal
#' @examples
#' \dontrun{
#' # Assuming OUTPUT_FILE environment variable is set
#' output_path <- outputFile()
#' }
outputFile <- function() {
    result <- Sys.getenv(x = "OUTPUT_FILE", "")
    logger.debug("outputFile: %s", result)
    result
}

#' Get Error File Path
#'
#' Retrieves the error file path from the ERROR_FILE environment variable.
#'
#' @return Character string containing the error file path, or empty string if
#'   the environment variable is not set
#' @export
#' @examples
#' \dontrun{
#' # Assuming ERROR_FILE environment variable is set
#' error_path <- errorFile()
#' }
errorFile <- function() {
  result <- Sys.getenv(x = "ERROR_FILE", "")
  logger.debug("errorFile: %s", result)
  result
}