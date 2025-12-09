
#' Execute MoveApps Application
#'
#' Simulates the execution of a MoveApps application by loading configuration,
#' reading input data, executing the main R function, and storing the result.
#' This function is the primary entry point for running MoveApps applications
#' in a production environment.
#'
#' @return No explicit return value. The function performs side effects including
#'   reading input data, executing the application function, and writing output files.
#'   In case of errors, the function logs the error and stores it to an error file
#'   before re-throwing the exception.
#'
#' @details The function performs the following operations in sequence:
#' \enumerate{
#'   \item Sets the system timezone to UTC for consistent time handling
#'   \item Loads application configuration using \code{\link{configuration}}
#'   \item Reads input data from the source file using \code{\link{readInput}}
#'   \item Adds the input data to the configuration arguments if data exists
#'   \item Executes the main application function (\code{rFunction}) with the arguments
#'   \item Stores the execution result to the output file using \code{\link{storeResult}}
#'   \item Handles any errors by logging them and storing to an error file
#' }
#'
#' The function expects the following to be available in the environment:
#' \itemize{
#'   \item \code{rFunction}: The main application function to be executed
#'   \item Environment variables configured for file paths (via \code{\link{sourceFile}},
#'         \code{\link{outputFile}}, \code{\link{errorFile}})
#' }
#'
#' @section Error Handling:
#' All errors are caught, logged using \code{print()}, and stored to the error file
#' specified by the \code{ERROR_FILE} environment variable. After logging, the
#' original exception is re-thrown to maintain proper error propagation.
#'
#' @seealso
#' \code{\link{configuration}} for loading app configuration,
#' \code{\link{readInput}} for reading input data,
#' \code{\link{storeResult}} for storing output,
#' \code{\link{sourceFile}}, \code{\link{outputFile}}, \code{\link{errorFile}} for file paths,
#' \code{\link{storeToFile}} for error logging
#'
#' @examples
#' \dontrun{
#' # Set up environment variables
#' Sys.setenv(SOURCE_FILE = "input.rds")
#' Sys.setenv(OUTPUT_FILE = "output.rds")
#' Sys.setenv(ERROR_FILE = "error.log")
#' Sys.setenv(CONFIGURATION = '{"param1": "value1"}')
#'
#' # Define the main application function
#' rFunction <- function(data, param1) {
#'   # Your app logic here
#'   return(processed_data)
#' }
#'
#' # Run the application
#' runMoveAppsApp()
#' }
#'
#' @export
# simulate an app run on moveapps.org
runMoveAppsApp <- function() {
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

#' Create MoveApps Shiny User Interface
#'
#' Creates the user interface for a MoveApps Shiny application simulation.
#' This function builds a fluid page layout with necessary JavaScript files,
#' CSS styles, UI components for the Shiny module interface, and WebSocket
#' connectivity enhancements for stable long-running operations.
#'
#' @param request A Shiny HTTP request object. This parameter follows Shiny's
#'   standard UI function signature for bookmark-enabled applications but is
#'   not actively used in the current implementation.
#'
#' @return A \code{\link[shiny]{fluidPage}} object containing the complete Shiny UI layout
#'   with embedded JavaScript files, CSS styles, module interface, and
#'   bookmark functionality.
#'
#' @details The function creates a UI that includes the following components:
#' \itemize{
#'   \item WebSocket keep-alive JavaScript fix and associated CSS for connection stability
#'   \item Custom JavaScript for extracting Shiny input values to JSON format
#'   \item The main Shiny module user interface via \code{shinyModuleUserInterface}
#'   \item WebSocket heartbeat text output for maintaining connection during long operations
#'   \item A bookmark button for saving application settings with custom styling
#' }
#'
#' The WebSocket fixes are implemented to address known issues with Shiny
#' WebSocket connections timing out during long-running operations, which is
#' common in data processing applications.
#'
#' @section Required Functions:
#' This function expects the following to be defined by the app developer:
#' \itemize{
#'   \item \code{shinyModuleUserInterface}: Function that returns the UI for the app's Shiny module
#' }
#'
#' @section Static Resources:
#' The function loads the following bundled resources:
#' \itemize{
#'   \item \code{ws-keep-alive-fix.js}: JavaScript for WebSocket connection maintenance
#'   \item \code{ws-keep-alive-fix.css}: CSS styles for WebSocket fix components
#'   \item \code{extract-inputs.js}: JavaScript for extracting input values to JSON
#' }
#'
#' @seealso
#' \code{\link{createMoveAppsShinyServer}} for the corresponding server function,
#' \code{\link[shiny]{fluidPage}}, \code{\link[shiny]{includeScript}}, \code{\link[shiny]{includeCSS}},
#' \code{\link[shiny]{textOutput}}, \code{\link[shiny]{bookmarkButton}}
#'
#' @examples
#' \dontrun{
#' # Define the module UI function
#' shinyModuleUserInterface <- function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     sliderInput(ns("slider"), "Value:", min = 1, max = 100, value = 50),
#'     plotOutput(ns("plot"))
#'   )
#' }
#'
#' # Use in a Shiny application
#' ui <- createMoveAppsShinyUI
#' server <- createMoveAppsShinyServer
#' shinyApp(ui = ui, server = server)
#' }
#'
#' @export
createMoveAppsShinyUI <- function(request) {
  fluidPage(
    # include JS/CSS bundled into this R package
    # kudos: https://shiny.posit.co/r/articles/build/packaging-javascript/
    includeScript(system.file("shiny-apps/www/ws-keep-alive-fix.js", package = "moveapps")),
    includeCSS(system.file("shiny-apps/www/ws-keep-alive-fix.css", package = "moveapps")),
    includeScript(system.file("shiny-apps/www/extract-inputs.js", package = "moveapps")),
    
    # ws-heartbeat fix
    # kudos: https://github.com/rstudio/shiny/issues/2110#issuecomment-419971302
    textOutput("ws_heartbeat"),
    
    # store the current state (as a shiny bookmark)
    tags$div(
      style = "position: relative; display: flex; justify-content: space-between; align-items: center; padding: 15px 20px;  border-bottom: none; margin-bottom: 0;",
      h2(""),
      bookmarkButton(id = 'ma_bookmark', label="Store settings", title="Click here to store the current chosen settings for future runs of the workflow",class="btn btn-outline-success", style = "margin: 0;")
    ),
    
    # functions (`shinyModuleUserInterface()` and `shinyModule()`) provided by the app developer
    shinyModuleUserInterface("shinyModule")
  )
}

#' Create MoveApps Shiny Server Function
#'
#' Implements the server logic for a MoveApps Shiny application simulation.
#' This function handles data reading, module execution, bookmark management,
#' result storage, error handling, and WebSocket connectivity management for
#' the Shiny application.
#'
#' @param input Standard Shiny server input object containing reactive values
#'   from UI components.
#' @param output Standard Shiny server output object for sending data to the UI.
#' @param session Standard Shiny server session object for managing the user session,
#'   including bookmarks, custom messages, and application lifecycle.
#'
#' @return No explicit return value. The function sets up reactive observers
#'   and handles all server-side logic through side effects including file I/O,
#'   bookmark management, and application state management.
#'
#' @details The function performs the following operations:
#' \itemize{
#'   \item Reads input data using \code{\link{readInput}} from the configured source file
#'   \item Calls the Shiny module (\code{shinyModule}) with data if available
#'   \item Automatically restores bookmarks from previous sessions on startup
#'   \item Handles bookmark creation when the bookmark button is clicked
#'   \item Extracts and saves Shiny input values as JSON for external access
#'   \item Stores computation results to output file when processing completes
#'   \item Implements WebSocket heartbeat mechanism for connection stability
#'   \item Provides comprehensive error handling with logging and appropriate app termination
#' }
#'
#' @section Bookmark Management:
#' The function implements a dual bookmarking system:
#' \itemize{
#'   \item Native Shiny RDS bookmarking for session restoration
#'   \item Custom JSON bookmarking for external access to input values
#'   \item Automatic bookmark restoration on application startup
#'   \item Exclusion of the bookmark button itself from saved state
#' }
#'
#' @section Error Handling:
#' Errors are handled with different strategies based on error codes:
#' \itemize{
#'   \item Error code 10: Triggers application termination via \code{\link[shiny]{stopApp}}
#'   \item Other errors: Logged and re-thrown for proper error propagation
#'   \item All errors are stored to the error file for debugging
#' }
#'
#' @section Required Functions:
#' This function expects the following to be defined by the app developer:
#' \itemize{
#'   \item \code{shinyModule}: The main Shiny module server function
#' }
#'
#' @seealso
#' \code{\link{createMoveAppsShinyUI}} for the corresponding UI function,
#' \code{\link{readInput}} for data input,
#' \code{\link{storeResult}} for result storage,
#' \code{\link{restoreShinyBookmark}}, \code{\link{saveBookmarkAsLatest}}, \code{\link{saveInputAsJson}} for bookmark management,
#' \code{\link{storeToFile}} for error logging,
#' \code{\link{notifyDone}}, \code{\link{notifyPushBookmark}} for external notifications
#'
#' @examples
#' \dontrun{
#' # Define the module server function
#' shinyModule <- function(input, output, session, data = NULL) {
#'   output$plot <- renderPlot({
#'     plot(1:input$slider)
#'   })
#'
#'   return(reactive({
#'     # Return processed data
#'     list(slider_value = input$slider, timestamp = Sys.time())
#'   }))
#' }
#'
#' # Use in a Shiny application
#' ui <- createMoveAppsShinyUI
#' server <- createMoveAppsShinyServer
#' shinyApp(ui = ui, server = server, enableBookmarking = "server")
#' }
#'
#' @export
createMoveAppsShinyServer <- function(input, output, session) {
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