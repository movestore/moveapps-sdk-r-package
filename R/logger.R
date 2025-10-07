
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
#' @param ... Additional arguments to be passed to sprintf for message formatting
#'
#' @return A formatted character string containing timestamp, log level, and message
#'
#' @examples
#' \dontrun{
#' logger.layout(INFO, "Application started")
#' logger.layout(DEBUG, "Processing %d items", 42)
#' }
#'
#' @seealso \code{\link{sprintf}} for message formatting details
logger.layout <- function(level, msg, ...) {
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) if(is.null(x)) 'NULL' else x )
    msg <- do.call(sprintf, c(msg, parsed))
  }
  sprintf("%s [%-5s] %s\n", time, names(level), msg)
}

#' Core logging function with level threshold checking
#'
#' Internal logging function that determines whether to display a log message
#' based on the current logging threshold. This function reads the LOG_LEVEL_SDK
#' environment variable to get the current threshold and only outputs messages
#' if the provided level meets or exceeds the priority threshold.
#'
#' @param msg A character string containing the log message, supports sprintf formatting
#' @param ... Additional arguments passed to the layout function for sprintf-style message formatting
#' @param level An integer representing the log level to check against the threshold.
#'   Should be one of the predefined log level constants (FATAL, ERROR, WARN, INFO, DEBUG, TRACE)
#'
#' @details
#' This function implements the core logging logic by:
#' \enumerate{
#'   \item Reading the current log threshold from the LOG_LEVEL_SDK environment variable (defaults to "DEBUG")
#'   \item Comparing the provided level against this threshold
#'   \item Only formatting and displaying messages if level <= threshold
#' }
#'
#' The log level hierarchy (lower numbers = higher priority):
#' \itemize{
#'   \item FATAL (1) - Critical errors
#'   \item ERROR (2) - Error conditions
#'   \item WARN (4) - Warning conditions
#'   \item INFO (6) - Informational messages
#'   \item DEBUG (8) - Debug information
#'   \item TRACE (9) - Detailed trace information
#' }
#'
#' @return No return value, called for side effects (outputs to console via cat())
#'
#' @note This is an internal function typically called by the specific logging
#' functions (logger.trace, logger.debug, etc.) rather than directly by users.
#'
#' @examples
#' \dontrun{
#' # Typically called by other logging functions, but can be used directly
#' logger.log_level("Processing started", level = INFO)
#' logger.log_level("Found %d items", 42, level = DEBUG)
#'
#' # Set environment to only show warnings and above
#' Sys.setenv(LOG_LEVEL_SDK = "WARN")
#' logger.log_level("This won't show", level = DEBUG)  # Won't display
#' logger.log_level("This will show", level = WARN)    # Will display
#' }
#'
#' @seealso
#' \code{\link{logger.layout}} for message formatting,
#' \code{\link{logger.trace}}, \code{\link{logger.debug}}, \code{\link{logger.info}},
#' \code{\link{logger.warn}}, \code{\link{logger.error}}, \code{\link{logger.fatal}}
logger.log_level <- function(msg, ..., level)
{
  if (level <= .logger_env$threshold) {
    message <- logger.layout(level, msg, ...)
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
#' @export
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
#' @export
#' @examples
#' logger.debug("Processing item %d of %d", 5, 10)
#' logger.debug("Current state: %s", "waiting")
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
#' @export
#' @examples
#' logger.info("Application started successfully")
#' logger.info("Processing completed: %d items processed", 58)
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
#' @export
#' @examples
#' logger.warn("Configuration file not found, using defaults")
#' logger.warn("Memory usage is high: %d MB", 12)
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
#' @export
#' @examples
#' logger.error("Failed to connect to database: %s", "foo")
#' logger.error("Invalid input provided: %s", "invalid")
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
#' @export
#' @examples
#' logger.fatal("Critical system error: %s", "low resources")
#' logger.fatal("Application cannot continue due to: %s", "panic")
#'
#' @seealso \code{\link{logger.log_level}}, \code{\link{FATAL}}
logger.fatal <- function(msg, ...) {
  logger.log_level(msg, ..., level = FATAL)
}

#' Logger environment for storing configuration
#'
#' Internal environment to store logger configuration variables
#' like the current threshold level. This prevents issues with
#' locked bindings in package namespaces.
.logger_env <- new.env(parent = emptyenv())
.logger_env$threshold <- DEBUG  # Default threshold

#' Initialize the logger with threshold from environment or parameter
#'
#' Initializes the logging system by configuring the logger threshold. You can
#' control the threshold via the LOG_LEVEL_SDK environment variable or by
#' passing the optional threshold_param argument.
#' This function should be called once at the start of your application to set up
#' the logging behavior.
#'
#' @param threshold_param Optional. A character level name (e.g., "INFO") or one of
#'   the level constants (e.g., INFO) to be used as threshold when the environment
#'   variable is not set.
#'
#' @details
#' Final threshold resolution priority:
#' \enumerate{
#'   \item If environment variable LOG_LEVEL_SDK is set, use it.
#'   \item Otherwise, if threshold_param is provided, use it.
#'   \item Otherwise, fall back to "DEBUG".
#' }
#'
#' The function searches for log level constants in the current package namespace first,
#' then falls back to the global environment if needed.
#'
#' Valid values are:
#' \itemize{
#'   \item "FATAL" - Only critical errors (level 1)
#'   \item "ERROR" - Errors and above (level 2)
#'   \item "WARN" - Warnings and above (level 4)
#'   \item "INFO" - Informational messages and above (level 6)
#'   \item "DEBUG" - Debug messages and above (level 8, default)
#'   \item "TRACE" - All messages including trace (level 9)
#' }
#'
#' @return No return value, called for side effects (sets internal logger threshold)
#'
#' @note If an invalid value is provided (environment or parameter), the function
#' will issue a warning and fall back to DEBUG level logging.
#'
#' @examples
#' # Initialize with default DEBUG level
#' logger.init()
#'
#' # Set environment variable and initialize
#' Sys.setenv(LOG_LEVEL_SDK = "INFO")
#' logger.init()
#'
#' # Or set via parameter (useful in tests)
#' logger.init(threshold_param = "WARN")
#' logger.init(threshold_param = INFO)
#'
#' # Example with invalid level (will warn and use DEBUG)
#' Sys.setenv(LOG_LEVEL_SDK = "INVALID")
#' logger.init()  # Warning issued, DEBUG level used
#'
#' @seealso
#' \code{\link{logger.log_level}} for the core logging function that uses this threshold,
#' \code{\link{Sys.getenv}} for environment variable access,
#' Log level constants: \code{\link{FATAL}}, \code{\link{ERROR}}, \code{\link{WARN}},
#' \code{\link{INFO}}, \code{\link{DEBUG}}, \code{\link{TRACE}}
#'
#' @export
logger.init <- function(threshold_param = NULL) {
  # Determine threshold source with priority:
  # 1) Environment variable LOG_LEVEL_SDK (if set)
  # 2) Optional function parameter threshold_param
  # 3) Fallback to "DEBUG"

  resolve_level_by_name <- function(name) {
    tryCatch({
      get(name, envir = environment())
    }, error = function(e) {
      tryCatch({
        get(name, envir = .GlobalEnv)
      }, error = function(e2) {
        warning(sprintf("Invalid log level name '%s', using DEBUG", name))
        DEBUG
      })
    })
  }

  resolve_level_by_value <- function(val) {
    # Map numeric values to known constants to preserve names()
    levels_map <- c(FATAL = FATAL, ERROR = ERROR, WARN = WARN, INFO = INFO, DEBUG = DEBUG, TRACE = TRACE)
    idx <- which(unname(levels_map) == as.integer(val))
    if (length(idx) == 1) {
      get(names(levels_map)[idx], envir = environment())
    } else {
      warning(sprintf("Invalid numeric log level '%s', using DEBUG", as.character(val)))
      DEBUG
    }
  }

  env_val <- Sys.getenv("LOG_LEVEL_SDK", unset = NA)
  if (!is.na(env_val) && nzchar(env_val)) {
    # Priority 1: environment variable
    chosen_level <- resolve_level_by_name(env_val)
  } else if (!is.null(threshold_param)) {
    # Priority 2: function parameter
    if (is.character(threshold_param) && nzchar(threshold_param[1])) {
      chosen_level <- resolve_level_by_name(threshold_param[1])
    } else if (is.numeric(threshold_param)) {
      chosen_level <- resolve_level_by_value(threshold_param[1])
    } else {
      warning("Invalid threshold_param type, using DEBUG")
      chosen_level <- DEBUG
    }
  } else {
    # Priority 3: fallback
    chosen_level <- DEBUG
  }

  .logger_env$threshold <- chosen_level
  cat(paste('logger threshold set to', names(.logger_env$threshold), '\n'))
}
