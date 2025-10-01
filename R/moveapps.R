
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
execute <- function() {
    tryCatch(
    {
        Sys.setenv(tz="UTC")

        args <- moveapps::configuration()
        data <- moveapps::readInput(moveapps::sourceFile())
        if (!is.null(data)) {
            args[["data"]] <- data
        }

        result <- do.call(rFunction, args)
        moveapps::storeResult(result, moveapps::outputFile())
    },
    error = function(e)
    {
        # error handler picks up where error was generated
        print(paste("ERROR: ", e))
        moveapps::storeToFile(e, moveapps::errorFile())
        stop(e) # re-throw the exception
    })
}

#' Simulate MoveApps Shiny UI
#'
#' Creates the user interface for a MoveApps Shiny application simulation.
#' This function builds a fluid page layout with necessary JavaScript files,
#' CSS styles, and UI components for the Shiny module interface.
#'
#' @param request A Shiny HTTP request object. This parameter follows Shiny's
#'   standard UI function signature but is not actively used in the implementation.
#'
#' @return A \code{fluidPage} object containing the complete Shiny UI layout
#'   with embedded JavaScript files, CSS styles, module interface, and
#'   bookmark functionality.
#'
#' @details The function creates a UI that includes:
#' \itemize{
#'   \item WebSocket keep-alive JavaScript fix and associated CSS
#'   \item Custom JavaScript for extracting Shiny input values
#'   \item The main Shiny module user interface via \code{shinyModuleUserInterface}
#'   \item WebSocket heartbeat text output for connection stability
#'   \item A bookmark button for saving application settings
#' }
#'
#' The WebSocket fixes are implemented to address known issues with Shiny
#' WebSocket connections timing out during long-running operations.
#'
#' @seealso \code{\link{simulateMoveAppsShinyServer}}, \code{\link{shinyModuleUserInterface}}
#'
#' @examples
#' \dontrun{
#' # Use in a Shiny application
#' ui <- simulateMoveAppsShinyUi
#' }
#'
#' @export
simulateMoveAppsShinyUi <- function(request) {
  fluidPage(
    # include JS/CSS bundled into this R package
    # kudos: https://shiny.posit.co/r/articles/build/packaging-javascript/
    includeScript(system.file("shiny-apps/www/ws-keep-alive-fix.js", package = "moveapps")),
    includeCSS(system.file("shiny-apps/www/ws-keep-alive-fix.css", package = "moveapps")),
    includeScript(system.file("shiny-apps/www/extract-inputs.js", package = "moveapps")),

    # functions (`shinyModuleUserInterface()` and `shinyModule()`) provided by the app developer
    shinyModuleUserInterface("shinyModule"),

    # ws-heartbeat fix
    # kudos: https://github.com/rstudio/shiny/issues/2110#issuecomment-419971302
    textOutput("ws_heartbeat"),
    # store the current state (as a shiny bookmark)
    bookmarkButton(id = 'ma_bookmark', label="Store settings", title="Click here to store the current chosen settings for future runs of the workflow",class="btn btn-outline-success"),
  )
}

#' Simulate MoveApps Shiny Server
#'
#' Implements the server logic for a MoveApps Shiny application simulation.
#' This function handles data reading, module execution, bookmark management,
#' result storage, and error handling for the Shiny application.
#'
#' @param input Standard Shiny server input object containing reactive values
#'   from UI components.
#' @param output Standard Shiny server output object for sending data to UI.
#' @param session Standard Shiny server session object for managing the user session.
#'
#' @return No explicit return value. The function sets up reactive observers
#'   and handles all server-side logic through side effects.
#'
#' @details The function performs the following operations:
#' \itemize{
#'   \item Reads input data using \code{readInput(sourceFile())}
#'   \item Calls the Shiny module with data if available
#'   \item Restores bookmarks from previous sessions automatically
#'   \item Handles bookmark creation when the bookmark button is clicked
#'   \item Extracts and saves Shiny input as JSON for external access
#'   \item Stores results to output file when computation completes
#'   \item Implements WebSocket heartbeat for connection stability
#'   \item Provides comprehensive error handling with logging and app termination
#' }
#'
#' The function includes special handling for error code 10, which triggers
#' application termination, while other errors are re-thrown after logging.
#'
#' @seealso \code{\link{simulateMoveAppsShinyUi}}, \code{\link{readInput}},
#'   \code{\link{storeResult}}, \code{\link{restoreShinyBookmark}},
#'   \code{\link{saveInputAsJson}}
#'
#' @examples
#' \dontrun{
#' # Use in a Shiny application
#' server <- simulateMoveAppsShinyServer
#'
#' # Run the complete app
#' shinyApp(ui = simulateMoveAppsShinyUi, server = simulateMoveAppsShinyServer)
#' }
#'
#' @export
simulateMoveAppsShinyServer <- function(input, output, session) {
  tryCatch(
  {
    data <- moveapps::readInput(moveapps::sourceFile())
    shinyModuleArgs <- c(shinyModule, "shinyModule")
    if (!is.null(data)) {
      shinyModuleArgs[["data"]] <- data
    }

    result <- do.call(callModule, shinyModuleArgs)

    observeEvent(
      session,
      {
        moveapps::restoreShinyBookmark(session)
        # Trigger extractShinyInput after restoring the bookmark
        session$sendCustomMessage("extract-shiny-input", list())
      },
      once = TRUE
    )

    # Need to exclude the button itself from being bookmarked
    setBookmarkExclude(c("ma_bookmark"))
    # Trigger bookmarking with button (needed b/c of custom bookmark button ID)
    observeEvent(input$ma_bookmark, {
      # call the (native shiny) RDS bookmarking
      session$doBookmark()
      # call the (custom MoveApps) JSON bookmarking
      session$sendCustomMessage("extract-shiny-input", list())
    })
    # listen to the custom shiny input extraction and store it as JSON
    observeEvent(input$shiny_input_json, {
      req(input$shiny_input_json)
      moveapps::saveInputAsJson(input$shiny_input_json)
      moveapps::notifyPushBookmark("input.json")
    })
    observe({
      moveapps::storeResult(result(), moveapps::outputFile())
      moveapps::notifyDone("SHINY")
    })
  },
  error = function(e) {
    # error handler picks up where error was generated
    print(paste("ERROR: ", e))
    moveapps::storeToFile(e, moveapps::errorFile())
    if (grepl("[code 10]", e$message, fixed=TRUE)) {
      stopApp(10)
    } else {
      stop(e) # re-throw the exception
    }
  })

  # ws-heartbeat fix
  # kudos: https://github.com/rstudio/shiny/issues/2110#issuecomment-419971302
  output$ws_heartbeat <- renderText({
    req(input$heartbeat)
    input$heartbeat
  })

  # hook after persisting the bookmark
  # see https://shiny.rstudio.com/articles/advanced-bookmarking.html
  onBookmarked(function(url) {
    moveapps::saveBookmarkAsLatest(url)
    moveapps::notifyPushBookmark("input.rds")
  })
}
