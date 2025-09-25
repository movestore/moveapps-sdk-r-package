#' Read RDS Input File
#'
#' Reads an RDS file from the specified source file path. This function handles
#' validation of the input file, including checking for empty files and null inputs.
#' It provides appropriate error handling and logging for various edge cases.
#'
#' @param sourceFile A character string specifying the path to the RDS file to read.
#'   Can be NULL or an empty string, in which case the function returns NULL.
#'
#' @return The deserialized R object from the RDS file, or NULL if no valid source
#'   file is provided or if the source file parameter is NULL/empty.
#'
#' @details
#' The function performs the following validations:
#' \itemize{
#'   \item Checks if sourceFile is not NULL and not an empty string
#'   \item Validates that the file is not empty (size > 0 bytes)
#'   \item Logs debug information about the reading process
#' }
#'
#' If an empty file is encountered, the function throws an error with code 10,
#' indicating invalid NULL input that cannot be processed.
#'
#' @examples
#' \dontrun{
#' # Read a valid RDS file
#' data <- readRdsInput("path/to/data.rds")
#'
#' # Handle NULL input
#' result <- readRdsInput(NULL)  # Returns NULL
#'
#' # Handle empty string input
#' result <- readRdsInput("")    # Returns NULL
#' }
#'
#' @seealso \code{\link{readRDS}}, \code{\link{storeRdsOutput}}
#'
#' @export
readRdsInput <- function(sourceFile) {
  if(!is.null(sourceFile) && sourceFile != "") {
    if (file.info(sourceFile)$size == 0) {
        # handle the special `null`-input
        logger.warn("The App has received invalid input! It cannot process NULL-input. Aborting..")
        # codes for exception handling
        # 10: abort consuming null-input
        stop("The App has received invalid input! It cannot process NULL-input. Check the output of the preceding App or adjust the datasource configuration. [code 10]")
    }
    logger.debug("Reading RDS from file '%s'", sourceFile)
    rds <- readRDS(file = sourceFile)
    return(rds)
  } else {
    logger.debug("Skip loading: no source File")
  }

  return(NULL)
}

#' Store RDS Output File
#'
#' Saves an R object to an RDS file at the specified output file path. This function
#' handles both valid results and NULL results, creating appropriate output files
#' in each case with proper logging.
#'
#' @param result The R object to be serialized and saved to the RDS file. Can be
#'   any R object or NULL.
#' @param outputFile A character string specifying the path where the RDS file
#'   should be saved. Can be NULL or an empty string.
#'
#' @return This function is called for its side effects (file creation) and does
#'   not return a value.
#'
#' @details
#' The function behavior depends on the input parameters:
#' \itemize{
#'   \item If outputFile is valid (not NULL/empty) and result is not NULL:
#'         saves the result object as an RDS file
#'   \item If result is NULL or outputFile is NULL/empty: creates an empty file
#'         at the outputFile path for post-processing purposes
#' }
#'
#' All operations are logged at the debug level for monitoring and troubleshooting.
#'
#' @examples
#' \dontrun{
#' # Store a valid R object
#' my_data <- data.frame(x = 1:10, y = letters[1:10])
#' storeRdsOutput(my_data, "output/result.rds")
#'
#' # Handle NULL result (creates empty file)
#' storeRdsOutput(NULL, "output/empty_result.rds")
#'
#' # Handle invalid output file path
#' storeRdsOutput(my_data, NULL)  # No file created
#' }
#'
#' @seealso \code{\link{saveRDS}}, \code{\link{readRdsInput}}, \code{\link{file.create}}
#'
#' @export
storeRdsOutput <- function(result, outputFile) {
    if(!is.null(outputFile) && outputFile != "" && !is.null(result)) {
        logger.debug("Storing RDS to file '%s'", outputFile)
        saveRDS(result, file = outputFile)
    } else {
        logger.debug("Storing the null-result to file %s", outputFile)
        file.create(outputFile) # write an empty file (for post-processing etc.)
    }
}