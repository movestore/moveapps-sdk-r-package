// Reads the value of a bound shiny input the way shiny sends it to the server: through its input
// binding, not the HTML elements (a checkbox group, a date range or radio buttons have no single
// element carrying the value). Shared by `extract-inputs.js` and `unsaved-settings-warning.js`.
window.MoveAppsSettings = (function () {
    // name and size of each file of the last completed upload per input id, e.g. {upload: [["area.gpkg", 17]]}.
    // Shiny reports a completed upload with `inputType` "shiny.fileupload", then empties the element
    // (`val("")`); a failed upload never completes
    const completedUploads = {};
    $(document).on('shiny:inputchanged', function (event) {
        if (event.inputType === 'shiny.fileupload') {
            completedUploads[event.name] = event.value.map(function (file) {
                return [file.name, file.size];
            });
        }
    });

    // name and size of each uploaded file, e.g. [["area.gpkg", 17]]: of the last completed upload, or of
    // the restored one (shiny's `getValue()` knows only that one, null before and after a new upload)
    function uploadValue(el, binding) {
        if (completedUploads[el.id]) {
            return completedUploads[el.id];
        }
        const restored = binding.getValue(el);
        if (!restored) {
            return null;
        }
        const sizes = [].concat(restored.size);
        return [].concat(restored.name).map(function (name, i) {
            return [name, sizes[i]];
        });
    }

    // value of a bound shiny input (undefined for action buttons, e.g. "Store settings")
    function value(el) {
        if (!el.id || el.classList.contains('action-button')) {
            return undefined;
        }
        const binding = $(el).data('shiny-input-binding');
        if (!binding) {
            return undefined;
        }
        if (el.type === 'file') {
            return uploadValue(el, binding);
        }
        return binding.getValue(el);
    }

    return { value: value };
})();
