#' @export
wsKeepAliveJs <- function() {
  includeScript(system.file("shiny-apps/www/ws-keep-alive-fix.js", package = "moveapps"))
}