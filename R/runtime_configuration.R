#' Load Application Configuration
#'
#' Loads configuration from a JSON file specified by the CONFIGURATION
#' environment variable. Optionally prints the configuration with masked
#' sensitive settings based on environment variables.
#'
#' @return A list containing the parsed JSON configuration, or NULL if no
#'   configuration file is specified or if the file cannot be read.
#'
#' @details
#' The function reads the following environment variables:
#' \itemize{
#'   \item \code{CONFIGURATION}: Path to the JSON configuration file or json string
#'   \item \code{PRINT_CONFIGURATION}: Set to "yes" to print configuration on load
#'   \item \code{MASK_SETTING_IDS}: Comma-separated list of setting IDs to mask when printing
#' }
#'
#' When \code{PRINT_CONFIGURATION} is set to "yes", the configuration will be
#' logged using \code{logger.info()}. Any setting IDs listed in
#' \code{MASK_SETTING_IDS} will be replaced with "***masked***" in the printed output
#' for security purposes.
#'
#' @examples
#' \dontrun{
#' # Set environment variables
#' Sys.setenv(CONFIGURATION = "{\"key\": \"value\"}")
#' Sys.setenv(PRINT_CONFIGURATION = "yes")
#' Sys.setenv(MASK_SETTING_IDS = "password,api_key")
#'
#' # Load configuration
#' config <- configuration()
#' }
#'
#' @export
configuration <- function() {
    configurationString <- Sys.getenv(x = "CONFIGURATION", "{}")

    result <- if(configurationString != "") {
        jsonlite::fromJSON(txt=configurationString)
    } else {
        NULL
    }

    if (Sys.getenv(x = "PRINT_CONFIGURATION", "no") == "yes") {
        maskSettingIds <- strsplit(Sys.getenv(x = "MASK_SETTING_IDS", ""), ",")[[1]]
        filteredResult <- result
        if (length(maskSettingIds) > 0) {
            filteredResult[maskSettingIds] <- "***masked***"
        }
        logger.info("app will be started with configuration:\n%s", jsonlite::toJSON(filteredResult, auto_unbox = TRUE, pretty = TRUE))
    }
    result
}

#' Clear Recent Application Output
#'
#' Clears application output files and directories based on environment variable
#' configuration. This function is typically used for cleanup purposes when
#' starting a fresh application session.
#'
#' @return None (invisible NULL). This function is called for its side effects.
#'
#' @details
#' The function reads the following environment variables:
#' \itemize{
#'   \item \code{CLEAR_OUTPUT}: Set to "yes" to enable output clearing
#'   \item \code{APP_ARTIFACTS_DIR}: Path to the artifacts directory to clear and recreate
#'   \item \code{OUTPUT_FILE}: Path to the output file to delete
#' }
#'
#' When \code{CLEAR_OUTPUT} is set to "yes", the function will:
#' \itemize{
#'   \item Delete and recreate the artifacts directory (if specified)
#'   \item Create a .keep file in the recreated artifacts directory
#'   \item Delete the output file (if specified)
#' }
#'
#' All operations are logged using \code{logger.info()}.
#'
#' @examples
#' \dontrun{
#' # Set environment variables
#' Sys.setenv(CLEAR_OUTPUT = "yes")
#' Sys.setenv(APP_ARTIFACTS_DIR = "/path/to/artifacts")
#' Sys.setenv(OUTPUT_FILE = "/path/to/output.log")
#'
#' # Clear recent output
#' clearRecentOutput()
#' }
#'
#' @export
clearRecentOutput <- function() {
    if (Sys.getenv(x = "CLEAR_OUTPUT", "no") == "yes") {
        logger.info("Clearing recent output")
        # delete and recreate artifact directory if it exists
        artifact_dir <- Sys.getenv(x = "APP_ARTIFACTS_DIR", "")
        if (artifact_dir != "") {
            unlink(artifact_dir, recursive = TRUE)
            dir.create(artifact_dir)
            file.create(file.path(artifact_dir, ".keep"))
        }
        # delete app output file
        output_file <- Sys.getenv(x = "OUTPUT_FILE", "")
        if (output_file != "") {
            unlink(output_file)
        }
    }
}