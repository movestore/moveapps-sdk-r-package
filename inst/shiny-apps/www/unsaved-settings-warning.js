// Shows a warning while the settings in the UI differ from the stored settings (the latest bookmark).
$(function () {
    const warningId = 'ma_unsaved_settings';
    // values of all settings at the time they were stored (or the app was loaded)
    let storedSettings = null;
    // stored settings before the latest click on "Store settings" (to fall back to if storing fails)
    let previousStoredSettings = null;

    // current value of every bound shiny input, except action buttons (e.g. "Store settings")
    function currentSettings() {
        const settings = {};
        document.querySelectorAll('.shiny-bound-input').forEach(function (el) {
            if (!el.id || el.classList.contains('action-button')) {
                return;
            }
            const binding = $(el).data('shiny-input-binding');
            if (!binding) {
                return;
            }
            settings[el.id] = JSON.stringify(binding.getValue(el));
        });
        return settings;
    }

    function rememberStoredSettings() {
        storedSettings = currentSettings();
        updateWarning();
    }

    function updateWarning() {
        const warning = document.getElementById(warningId);
        if (!warning || storedSettings === null) {
            return;
        }
        const settings = currentSettings();
        let changed = false;
        Object.keys(settings).forEach(function (id) {
            if (!(id in storedSettings)) {
                // input appeared later (e.g. via renderUI): take its initial value as stored
                storedSettings[id] = settings[id];
            } else if (storedSettings[id] !== settings[id]) {
                changed = true;
            }
        });
        warning.style.display = changed ? '' : 'none';
    }

    // the first idle state after connecting: UI (incl. restored bookmark and server-side updates) is settled
    $(document).one('shiny:idle', rememberStoredSettings);
    // re-check after every input change (deferred, so the DOM reflects the new value)
    $(document).on('shiny:inputchanged shiny:bound', function () {
        setTimeout(updateWarning, 0);
    });
    // hide the warning as soon as "Store settings" is clicked
    $(document).on('click', '#ma_bookmark', function () {
        if (storedSettings === null) {
            return;
        }
        previousStoredSettings = storedSettings;
        rememberStoredSettings();
    });
    // the server confirms that the settings were stored
    Shiny.addCustomMessageHandler('ma-settings-stored', rememberStoredSettings);
    // storing failed: show the warning again
    Shiny.addCustomMessageHandler('ma-settings-store-failed', function (message) {
        if (previousStoredSettings !== null) {
            storedSettings = previousStoredSettings;
            updateWarning();
        }
    });
});
