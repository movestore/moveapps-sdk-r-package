// Shows a warning while the settings in the UI differ from the stored settings (the latest bookmark).
$(function () {
    const warningId = 'ma_unsaved_settings';
    // values of all settings at the time they were stored (or this App was loaded)
    let storedSettings = null;
    // ids of the settings which differ from the stored ones (kept while an input is re-rendered)
    const changedIds = new Set();
    // the latest attempt to store the settings failed: show the warning until storing succeeds
    let storeFailed = false;

    // value of a bound shiny input (undefined for action buttons, e.g. "Store settings")
    function settingValue(el) {
        if (!el.id || el.classList.contains('action-button')) {
            return undefined;
        }
        const binding = $(el).data('shiny-input-binding');
        if (!binding) {
            return undefined;
        }
        return JSON.stringify(binding.getValue(el));
    }

    // current value of every bound shiny input
    function currentSettings() {
        const settings = {};
        document.querySelectorAll('.shiny-bound-input').forEach(function (el) {
            const value = settingValue(el);
            if (value !== undefined) {
                settings[el.id] = value;
            }
        });
        return settings;
    }

    function rememberStoredSettings() {
        storedSettings = currentSettings();
        changedIds.clear();
        updateWarning();
    }

    function updateWarning() {
        const warning = document.getElementById(warningId);
        if (!warning || storedSettings === null) {
            return;
        }
        const settings = currentSettings();
        Object.keys(settings).forEach(function (id) {
            if (!(id in storedSettings)) {
                // fallback for an input which appeared without a `shiny:bound` event
                storedSettings[id] = settings[id];
            }
            if (storedSettings[id] !== settings[id]) {
                changedIds.add(id);
            } else {
                changedIds.delete(id);
            }
        });
        const changed = Object.keys(settings).some(function (id) {
            return changedIds.has(id);
        });
        warning.style.display = (changed || storeFailed) ? '' : 'none';
    }

    // the first idle state after connecting: UI (incl. restored bookmark and server-side updates) is settled
    $(document).one('shiny:idle', rememberStoredSettings);
    // an input (re)created by this App (e.g. via renderUI) shows what the next run shows as well:
    // take its initial value as stored, so it does not count as a change. An input which was changed
    // (and not stored) before it was re-rendered keeps its stored value, so the change is not hidden.
    $(document).on('shiny:bound', function (event) {
        if (event.bindingType === 'input' && storedSettings !== null && !changedIds.has(event.target.id)) {
            const value = settingValue(event.target);
            if (value !== undefined) {
                storedSettings[event.target.id] = value;
            }
        }
        setTimeout(updateWarning, 0);
    });
    // re-check after every input change (deferred, so the DOM reflects the new value)
    $(document).on('shiny:inputchanged', function () {
        setTimeout(updateWarning, 0);
    });
    // hide the warning as soon as "Store settings" is clicked
    $(document).on('click', '#ma_bookmark', function () {
        if (storedSettings === null) {
            return;
        }
        storeFailed = false;
        rememberStoredSettings();
    });
    // the server confirms that the settings were stored (and uploaded)
    Shiny.addCustomMessageHandler('ma-settings-stored', function (message) {
        storeFailed = false;
        updateWarning();
    });
    // storing failed: show the warning again (until storing succeeds)
    Shiny.addCustomMessageHandler('ma-settings-store-failed', function (message) {
        storeFailed = true;
        updateWarning();
    });
});
