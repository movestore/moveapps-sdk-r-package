
#' Fatal log level constant
#' 
#' Numeric constant representing the FATAL log level with value 1.
#' Used for logging critical errors that may cause the application to terminate.
FATAL <- 1L
names(FATAL) <- "FATAL"

#' Error log level constant
#' 
#' Numeric constant representing the ERROR log level with value 2.
#' Used for logging error conditions that don't necessarily terminate the application.
ERROR <- 2L
names(ERROR) <- "ERROR"

#' Warning log level constant
#' 
#' Numeric constant representing the WARN log level with value 4.
#' Used for logging warning conditions that should be addressed but don't prevent execution.
WARN <- 4L
names(WARN) <- "WARN"

#' Information log level constant
#' 
#' Numeric constant representing the INFO log level with value 6.
#' Used for logging general informational messages about application flow.
INFO <- 6L
names(INFO) <- "INFO"

#' Debug log level constant
#' 
#' Numeric constant representing the DEBUG log level with value 8.
#' Used for logging detailed information for debugging purposes.
DEBUG <- 8L
names(DEBUG) <- "DEBUG"

#' Trace log level constant
#' 
#' Numeric constant representing the TRACE log level with value 9.
#' Used for logging very detailed information, typically only of interest when diagnosing problems.
TRACE <- 9L
names(TRACE) <- "TRACE"

#' Format log messages with timestamp and level
#'
#' Creates a formatted log message string that includes the current timestamp,
#' log level name, and the formatted message. Supports sprintf-style formatting
#' with additional arguments.
#'
#' @param level An integer representing the log level (use predefined constants like INFO, DEBUG, etc.)
#' @param msg A character string containing the log message, potentially with sprintf format specifiers
#' @param id A character string identifier (currently unused in the implementation)
#' @param ... Additional arguments to be passed to sprintf for message formatting
#'
#' @return A formatted character string containing timestamp, log level, and message
#'
#' @examples
#' logger.layout(INFO, "Application started")
#' logger.layout(DEBUG, "Processing %d items", 42)
#'
#' @seealso \code{\link{sprintf}} for message formatting details
logger.layout <- function(level, msg, id='', ...) {
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) if(is.null(x)) 'NULL' else x )
    msg <- do.call(sprintf, c(msg, parsed))
  }
  sprintf("[%s] %s\n", names(level), msg)
}

#' Core logging function with level threshold checking
#'
#' Internal logging function that checks if the provided log level meets the
#' current logging threshold before formatting and outputting the message.
#' Messages are only displayed if the level is less than or equal to the threshold.
#'
#' @param msg A character string containing the log message
#' @param ... Additional arguments passed to the layout function for message formatting
#' @param level An integer representing the log level to check against the threshold
#'
#' @details This function compares the provided level against logger.threshold.
#' Only messages with levels less than or equal to the threshold are displayed.
#' Lower numeric values represent higher priority levels.
#'
#' @seealso \code{\link{logger.layout}} for message formatting
logger.log_level <- function(msg, ..., level)
{
  if (level <= logger.threshold)  {
    message <- logger.layout(level, msg, name, ...)
    cat(message)
  }
}

#' Log a trace level message
#'
#' Logs a message at the TRACE level, which is the most verbose logging level.
#' Trace messages are typically used for very detailed diagnostic information.
#'
#' @param msg A character string containing the log message, supports sprintf formatting
#' @param ... Additional arguments for sprintf-style message formatting
#'
#' @examples
#' logger.trace("Entering function with parameter: %s", "value")
#' logger.trace("Variable state: x = %d, y = %s", 42, "test")
#'
#' @seealso \code{\link{logger.log_level}}, \code{\link{TRACE}}
logger.trace <- function(msg, ...) {
  logger.log_level(msg, ..., level=TRACE)
}

#' Log a debug level message
#'
#' Logs a message at the DEBUG level, used for detailed information that is
#' typically only of interest when diagnosing problems.
#'
#' @param msg A character string containing the log message, supports sprintf formatting
#' @param ... Additional arguments for sprintf-style message formatting
#'
#' @examples
#' logger.debug("Processing item %d of %d", 5, 10)
#' logger.debug("Current state: %s", current_state)
#'
#' @seealso \code{\link{logger.log_level}}, \code{\link{DEBUG}}
logger.debug <- function(msg, ...) {
  logger.log_level(msg, ..., level=DEBUG)
}

#' Log an informational message
#'
#' Logs a message at the INFO level, used for general informational messages
#' that highlight the progress of the application.
#'
#' @param msg A character string containing the log message, supports sprintf formatting
#' @param ... Additional arguments for sprintf-style message formatting
#'
#' @examples
#' logger.info("Application started successfully")
#' logger.info("Processing completed: %d items processed", item_count)
#'
#' @seealso \code{\link{logger.log_level}}, \code{\link{INFO}}
logger.info <- function(msg, ...) {
  logger.log_level(msg, ..., level=INFO)
}

#' Log a warning message
#'
#' Logs a message at the WARN level, used for potentially harmful situations
#' that should be addressed but don't prevent the application from continuing.
#'
#' @param msg A character string containing the log message, supports sprintf formatting
#' @param ... Additional arguments for sprintf-style message formatting
#'
#' @examples
#' logger.warn("Configuration file not found, using defaults")
#' logger.warn("Memory usage is high: %d MB", memory_usage)
#'
#' @seealso \code{\link{logger.log_level}}, \code{\link{WARN}}
logger.warn <- function(msg, ...) {
  logger.log_level(msg, ..., level=WARN)
}

#' Log an error message
#'
#' Logs a message at the ERROR level, used for error events that might still
#' allow the application to continue running.
#'
#' @param msg A character string containing the log message, supports sprintf formatting
#' @param ... Additional arguments for sprintf-style message formatting
#'
#' @examples
#' logger.error("Failed to connect to database: %s", error_message)
#' logger.error("Invalid input provided: %s", user_input)
#'
#' @seealso \code{\link{logger.log_level}}, \code{\link{ERROR}}
logger.error <- function(msg, ...) {
  logger.log_level(msg, ..., level = ERROR)
}

#' Log a fatal error message
#'
#' Logs a message at the FATAL level, used for very severe error events that
#' might lead the application to abort or terminate.
#'
#' @param msg A character string containing the log message, supports sprintf formatting
#' @param ... Additional arguments for sprintf-style message formatting
#'
#' @examples
#' logger.fatal("Critical system error: %s", system_error)
#' logger.fatal("Application cannot continue due to: %s", fatal_condition)
#'
#' @seealso \code{\link{logger.log_level}}, \code{\link{FATAL}}
logger.fatal <- function(msg, ...) {
  logger.log_level(msg, ..., level = FATAL)
}

#' Global logging threshold level
#'
#' Determines the minimum log level that will be displayed. Messages with levels
#' higher than this threshold will be suppressed. The value is read from the
#' LOG_LEVEL_SDK environment variable, defaulting to TRACE if not set.
#'
#' @details The threshold uses numeric comparison where lower numbers represent
#' higher priority levels. For example:
#' \itemize{
#'   \item FATAL (1) - highest priority
#'   \item ERROR (2)
#'   \item WARN (4)
#'   \item INFO (6)
#'   \item DEBUG (8)
#'   \item TRACE (9) - lowest priority
#' }
#'
#' @seealso Environment variable LOG_LEVEL_SDK for configuration
logger.threshold = Sys.getenv(x = "LOG_LEVEL_SDK", TRACE)