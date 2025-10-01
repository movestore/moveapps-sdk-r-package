#' Notify Execution Done
#'
#' Sends a notification indicating that a specific type of execution has completed.
#' This function is part of the HTTP fake client implementation for testing and
#' debugging purposes.
#'
#' @param executionType Character string. The type of execution that has completed.
#'   This parameter identifies what kind of operation or process has finished.
#'
#' @return No return value, called for side effects (logging notification)
#'
#' @details
#' This is a fake client function used for testing and development. In the actual
#' implementation, this would typically send an HTTP notification to a server
#' endpoint. The fake implementation simply logs the notification using the
#' debug logging level.
#'
#' @examples
#' \dontrun{
#' notifyDone("data_processing")
#' notifyDone("model_training")
#' }
#'
#' @seealso \code{\link{logger.debug}} for the logging function used
#'
#' @export
#' @keywords internal
notifyDone <- function(executionType) {
  logger.debug("[http fake client] notify done")
}

#' Notify Push Bookmark
#'
#' Sends a notification for pushing a Shiny bookmark with the specified filename.
#' This function is part of the HTTP fake client implementation for testing and
#' debugging Shiny bookmark functionality.
#'
#' @param fileName Character string. The name of the file associated with the
#'   Shiny bookmark being pushed. This identifies which bookmark file is being
#'   processed or saved.
#'
#' @return No return value, called for side effects (logging notification)
#'
#' @details
#' This is a fake client function used for testing and development of Shiny
#' bookmark functionality. In the actual implementation, this would typically
#' send an HTTP notification to a server endpoint to handle bookmark persistence.
#' The fake implementation logs the notification with the filename for debugging purposes.
#'
#' Shiny bookmarks allow users to save the state of a Shiny application and
#' restore it later. This function would be called when a bookmark needs to be
#' pushed or synchronized with a remote service.
#'
#' @examples
#' \dontrun{
#' notifyPushBookmark("user_session_123.rds")
#' notifyPushBookmark("analysis_state.bookmark")
#' }
#'
#' @seealso
#' \code{\link{logger.debug}} for the logging function used,
#' \href{https://shiny.rstudio.com/articles/bookmarking-state.html}{Shiny Bookmarking}
#'
#' @export
#' @keywords internal
notifyPushBookmark <- function(fileName) {
  logger.debug(paste("[http fake client] notify push (shiny) bookmark", fileName))
}