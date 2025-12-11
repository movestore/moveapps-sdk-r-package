#' Get Auxiliary File Path
#'
#' Provides the path to an auxiliary file. Auxiliary files are files that:
#' \itemize{
#'   \item Are needed by the app to work during runtime
#'   \item Get uploaded by the user during app configuration time
#'   \item Will not generate warnings if the requested file is not available
#'         (e.g., the user did not upload anything and the app developer did not provide any fallback)
#'   \item Can also be provided by the app developer and get bundled into the app during build time (as fallback)
#' }
#'
#' @param appSpecUserFileSettingId Character string. The ID of the requested set of app-files
#'   (see counterpart in \code{appspec.json} at \code{setting[type=USER_FILE].id})
#' @param fallbackToProvidedFiles Logical. Should the function fallback to bundled directory
#'   of requested auxiliary file if the user did not upload anything? Default is \code{TRUE}.
#'
#' @return Character string containing the path to the requested file, or \code{NULL}
#'   if the user did not upload anything and no fallback was provided.
#'
#' @details
#' The function first checks for user-uploaded files in the user upload directory.
#' If no user files are found and \code{fallbackToProvidedFiles} is \code{TRUE},
#' it falls back to app developer provided files. The function expects exactly
#' one file in the target directory; if zero or more than one file is found,
#' it returns \code{NULL} and logs a warning.
#'
#' @examples
#' \dontrun{
#' # Get path to user-uploaded configuration file
#' config_path <- getAuxiliaryFilePath("config_file")
#'
#' # Get path without fallback to developer-provided files
#' user_data <- getAuxiliaryFilePath("user_data", fallbackToProvidedFiles = FALSE)
#' }
#'
#' @export
getAuxiliaryFilePath <- function(appSpecUserFileSettingId, fallbackToProvidedFiles = TRUE) {
    userUploadDir <- paste0(Sys.getenv(x = "USER_APP_FILE_HOME_DIR"), Sys.getenv(x = "USER_APP_FILE_UPLOAD_DIR", "/uploaded-app-files/"))
    appDevFallbackDir <- paste0(Sys.getenv(x = "USER_APP_FILE_HOME_DIR"), Sys.getenv(x = "USER_APP_FILE_FALLBACK_DIR", "/provided-app-files/"))
    dir <- getUploadDirOrFallbackDir(appSpecUserFileSettingId, fallbackToProvidedFiles, userUploadDir, appDevFallbackDir)
    if (is.null(dir)) {
        logger.warn("[%s] No files found for app setting. Therefore returning `NULL`.", appSpecUserFileSettingId)
        return(NULL)
    }
    if (length(list.files(dir)) == 0) {
        logger.info("[%s] The app setting of type `USER_FILE` contains '0' files (neither uploaded file nor provided file available). Therefore returning `NULL`.", appSpecUserFileSettingId)
        return(NULL)
    }
    if (length(list.files(dir)) != 1) {
        logger.warn("[%s] An app setting of type `USER_FILE` must contain exactly 0 or 1 file(s). The setting contains '%s' file(s). Therefore returning `NULL`.", appSpecUserFileSettingId, length(list.files(dir)))
        return(NULL)
    }
    # R vectors are one-based!
    result <- paste0(dir, list.files(dir)[1])
    logger.info("[%s] Resolved file-path: '%s'", appSpecUserFileSettingId, result)
    return(result)
}

#' Get Upload Directory or Fallback Directory
#'
#' Internal function that provides the path to the directory of an auxiliary file.
#' This function should not be used by app developers directly.
#' Use \code{\link{getAuxiliaryFilePath}} to get the path to the file instead of just the parent directory.
#'
#' @param appSpecUserFileSettingId Character string. The ID of the requested set of app-files
#' @param fallbackToProvidedFiles Logical. Should the function fallback to bundled directory?
#' @param userUploadDir Character string. Path to the user upload directory
#' @param appDevFallbackDir Character string. Path to the app developer fallback directory
#'
#' @return Character string containing the directory path, or \code{NULL} if no valid directory is found.
#'
#' @details
#' The function checks for user-uploaded files first. If none are found and
#' \code{fallbackToProvidedFiles} is \code{TRUE}, it falls back to app developer
#' provided files. If no files are found in either location, it returns \code{NULL}.
#'
#' @keywords internal
getUploadDirOrFallbackDir <- function(appSpecUserFileSettingId, fallbackToProvidedFiles, userUploadDir, appDevFallbackDir) {
    if (!is.null(appSpecUserFileSettingId) && appSpecUserFileSettingId != "") {
        userUpload <- paste0(userUploadDir, appSpecUserFileSettingId, "/")
        if (file.exists(userUpload) && length(list.files(userUpload)) > 0) {
            # directory exists and is not empty: user provided some files
            logger.debug("[%s] Detected files provided by user.", appSpecUserFileSettingId)
            return(userUpload)
        } else if (fallbackToProvidedFiles) {
            # fallback to directory provided by app developer
            logger.debug("[%s] Using fallback files provided by app developer.", appSpecUserFileSettingId)
            return(paste0(appDevFallbackDir, appSpecUserFileSettingId, "/"))
        } else {
            if (fallbackToProvidedFiles) {
                logger.warn("[%s] No files present for app-file-setting. User did not upload anything and the app did not provide fallback files.", appSpecUserFileSettingId)
            } else {
                logger.info("[%s] No files present for app-file-setting. User did not upload anything.", appSpecUserFileSettingId)
            }
            return(NULL)
        }
    }
}

#' Get App Artifact Path
#'
#' Provides the path to an app artifact. You can write app-generated files to this
#' path and they will become available on MoveApps after each app run.
#'
#' @param artifactName Character string. The name of the artifact
#'
#' @return Character string containing the full path for the artifact. Use this path
#'   to write your data.
#'
#' @details
#' App artifacts are files generated by your app that should be made available
#' to users after the app completes execution. Common examples include plots,
#' summary reports, or processed data files.
#'
#' @examples
#' \dontrun{
#' # Create a path for a plot artifact
#' plot_path <- appArtifactPath("movement_plot.png")
#'
#' # Save a plot to the artifact path
#' png(plot_path)
#' plot(x = 1:10, y = 1:10)
#' dev.off()
#'
#' # Create a path for a summary report
#' report_path <- appArtifactPath("analysis_summary.txt")
#' writeLines("Analysis complete", report_path)
#' }
#'
#' @export
appArtifactPath <- function(artifactName) {
    return(paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR"), artifactName))
}