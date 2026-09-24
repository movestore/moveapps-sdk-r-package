// Shows a warning while the settings in the UI differ from the stored settings (the latest bookmark).
// Also reports the settings shown in the UI once this App finished starting, so that the server can
// ignore stored settings which do not fit the input data.
$(function () {
    const warningId = 'ma_unsaved_settings';
    // values of all settings at the time they were stored (or this App was loaded)
    let storedSettings = null;
    // ids of the settings which differ from the stored ones (kept while an input is re-rendered)
    const changedIds = new Set();
    // the shown settings are known to differ from the stored ones (storing failed, or stored settings
    // which do not fit the input data were ignored): show the warning until storing succeeds
    let notStored = false;
    // until the user clicks or types for the first time, all changes are part of starting this App
    // (e.g. inputs filled by the server once the data is loaded) and are taken as stored
    let userInteracted = false;
    // the settings when a store was triggered (a click on "Store settings", or the report at the end of
    // the start, which stores requested default settings): they become the stored ones once the server
    // confirms, so a change made while the server stores and uploads stays a change
    let settingsBeingStored = null;

    // value of a bound shiny input (undefined for action buttons, e.g. "Store settings")
    function settingValue(el) {
        if (!el.id || el.classList.contains('action-button')) {
            return undefined;
        }
        const binding = $(el).data('shiny-input-binding');
        if (!binding) {
            return undefined;
        }
        if (el.type === 'file') {
            return JSON.stringify(uploadValue(el, binding));
        }
        return JSON.stringify(binding.getValue(el));
    }

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
        if (!userInteracted) {
            storedSettings = Object.assign({}, settings);
            changedIds.clear();
        }
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
        warning.style.display = (changed || notStored) ? '' : 'none';
    }

    // the server is computing (e.g. a setting created via `renderUI` from the data): this App has not
    // finished starting, whatever the timers say. Registered first, so that `busy` is up to date for
    // the other `shiny:idle` handlers
    let busy = false;
    let reportWhenIdle = false;
    $(document).on('shiny:busy', function () {
        busy = true;
        clearTimeout(startupTimer);
    });
    $(document).on('shiny:idle', function () {
        busy = false;
        if (reportWhenIdle) {
            reportWhenIdle = false;
            // shiny reports idle before it renders the outputs of that flush (measured: the idle event
            // comes ~10 ms before `shiny:bound` of a new setting); later activity must not postpone it
            setTimeout(reportStartupSettings, 500);
        }
    });

    // the first idle state after connecting: UI (incl. restored bookmark and server-side updates) is settled
    $(document).one('shiny:idle', function () {
        rememberStoredSettings();
        startupActivity();
        setTimeout(reportStartupSettings, 15000);
    });

    // this App finished starting once nothing happened for a while while the server is idle (at the
    // latest 15 s after the first idle, or at the first idle after that during a long computation)
    let startupTimer = null;
    let startupReported = false;
    function startupActivity() {
        if (startupReported || storedSettings === null || busy) {
            return;
        }
        clearTimeout(startupTimer);
        startupTimer = setTimeout(reportStartupSettings, 1500);
    }
    // report the ids of all settings shown, and of the ones the user already changed (the server leaves
    // those out; a click alone, e.g. on the map, does not stop the check of the others)
    function reportStartupSettings() {
        if (startupReported) {
            return;
        }
        if (busy) {
            reportWhenIdle = true;
            return;
        }
        startupReported = true;
        clearTimeout(startupTimer);
        updateWarning();
        const settings = currentSettings();
        settingsBeingStored = settings;
        Shiny.setInputValue('ma_startup_settings', {
            settingIds: Object.keys(settings),
            changedIds: Array.from(changedIds)
        }, {priority: 'event'});
    }
    $(document).on('shiny:idle shiny:bound', startupActivity);
    $(document).on('shiny:inputchanged', function (event) {
        if (event.name !== 'heartbeat' && event.name !== 'ma_startup_settings') {
            startupActivity();
        }
    });
    // the first real (trusted) click or key press of the user ends the start of this App; it happens
    // before the resulting change of a setting, so the settings at this moment are taken as stored
    function onFirstUserInteraction(event) {
        if (!event.isTrusted || userInteracted) {
            return;
        }
        if (storedSettings !== null) {
            rememberStoredSettings();
        }
        userInteracted = true;
    }
    document.addEventListener('pointerdown', onFirstUserInteraction, true);
    document.addEventListener('keydown', onFirstUserInteraction, true);
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
    // re-check after every change of a setting (deferred, so the DOM reflects the new value). Only bound
    // inputs are settings; `Shiny.setInputValue()` (heartbeat, map bounds, JSON extraction) has no binding
    $(document).on('shiny:inputchanged', function (event) {
        if (event.binding) {
            setTimeout(updateWarning, 0);
        }
    });
    // hide the warning as soon as "Store settings" is clicked
    $(document).on('click', '#ma_bookmark', function () {
        if (storedSettings === null) {
            return;
        }
        notStored = false;
        rememberStoredSettings();
        settingsBeingStored = Object.assign({}, storedSettings);
    });
    // the server confirms that the settings were stored (and uploaded)
    Shiny.addCustomMessageHandler('ma-settings-stored', function (message) {
        notStored = false;
        if (settingsBeingStored !== null && storedSettings !== null) {
            storedSettings = settingsBeingStored;
        }
        settingsBeingStored = null;
        updateWarning();
    });
    // the shown settings differ from the stored ones (storing failed, or stored settings which do not
    // fit the input data were ignored): show the warning until storing succeeds
    Shiny.addCustomMessageHandler('ma-settings-not-stored', function (message) {
        notStored = true;
        updateWarning();
    });
});
