
#' Simulate a MoveApps run
#'
#' This function simulates the execution of a MoveApps application by reading
#' input data, executing the main R function with provided arguments, and
#' storing the result. It handles errors by logging them and re-throwing the
#' exception.
#'
#' @param args A list of arguments to be passed to the main R function.
#'   The function will automatically add a "data" element to this list
#'   if input data is successfully read.
#'
#' @return The function doesn't return a value explicitly but stores the
#'   result of the R function execution to an output file.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Sets the timezone to UTC
#'   \item Reads input data using \code{readInput(sourceFile())}
#'   \item Adds the data to the arguments list if data is not NULL
#'   \item Executes the main R function using \code{do.call(rFunction, args)}
#'   \item Stores the result using \code{storeResult(result, outputFile())}
#'   \item Handles any errors by printing them and storing to an error file
#' }
#'
#' @seealso \code{\link{readInput}}, \code{\link{sourceFile}},
#'   \code{\link{storeResult}}, \code{\link{outputFile}},
#'   \code{\link{storeToFile}}, \code{\link{errorFile}}
#'
#' @examples
#' \dontrun{
#' # Simulate a MoveApps run with custom arguments
#' args <- list(parameter1 = "value1", parameter2 = 123)
#' simulateMoveAppsRun(args)
#' }
#'
#' @export
# simulate an app run on moveapps.org
simulateMoveAppsRun <- function(args) {
    tryCatch(
    {
        Sys.setenv(tz="UTC")

        data <- readInput(sourceFile())
        if (!is.null(data)) {
            args[["data"]] <- data
        }

        result <- do.call(rFunction, args)
        storeResult(result, outputFile())
    },
    error = function(e)
    {
        # error handler picks up where error was generated
        print(paste("ERROR: ", e))
        storeToFile(e, errorFile())
        stop(e) # re-throw the exception
    })
}