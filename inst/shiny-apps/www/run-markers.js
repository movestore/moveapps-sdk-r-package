// Writes the course of a run to the browser console. MoveApps' headless browser (co-pilot) reads
// these lines to close the page after `done`, or to fail the run when the App stops while opening.
// In a user's browser they are noise only.
Shiny.addCustomMessageHandler('ma-run-done', function (message) {
    console.info('ma-run:done');
});
$(document).on('shiny:disconnected', function () {
    // the restore reload ends the session on purpose (reload-page.js)
    if (!window.maReloading) {
        console.info('ma-run:disconnected');
    }
});
