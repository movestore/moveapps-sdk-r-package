// Loads this page again for the server (e.g. to restore the stored settings), as a new navigation.
// `location.reload()` (shiny's `session$reload()`) would let Firefox fill the reloaded page with the
// inputs of the page before, e.g. the default instead of the restored setting.
Shiny.addCustomMessageHandler('ma-reload', function (message) {
    window.location.replace(window.location.href);
});
