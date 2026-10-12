Shiny.addCustomMessageHandler('extract-shiny-input', extractShinyInput)

// every setting as {label, inputId, value} with the value shiny sends to the server (SDK issue #7);
// radio buttons and uploads also carry a display text as `valueLabel`
function extractShinyInput(message) {
    const results = [];
    document.querySelectorAll('.shiny-bound-input').forEach(function (el) {
        const value = MoveAppsSettings.value(el);
        if (value === undefined) {
            return;
        }
        const entry = {
            label: settingLabel(el),
            inputId: el.id,
            // groundcontrol requires a value: an empty setting (e.g. an upload not made yet, a numeric
            // input set to NA) is recorded as "" and not as null
            value: el.type === 'password' ? maskedValue(value) : (value === null ? '' : value),
        };
        const valueLabel = displayText(el, value);
        if (valueLabel !== undefined) {
            entry.valueLabel = valueLabel;
        }
        results.push(entry);
    });

    // submit JSON as STRING (avoids conversion to table at the shiny server)
    Shiny.setInputValue('shiny_input_json', JSON.stringify(results), {priority: 'event'});
}

// shiny renders a label with the id `<inputId>-label` (empty for `label = NULL`); a checkbox has none,
// its text sits in the `<label>` around it
function settingLabel(el) {
    const label = document.getElementById(el.id + '-label') || el.closest('label');
    return label ? label.textContent.trim() : '';
}

function maskedValue(value) {
    return value && value.length > 0 ? '***masked***' : '';
}

// what the user saw: the text of the chosen radio button, the names of the uploaded files
function displayText(el, value) {
    if (el.type === 'file') {
        return value ? value.map(function (file) { return file[0]; }).join(', ') : undefined;
    }
    const checkedRadio = el.querySelector('input[type="radio"]:checked');
    if (checkedRadio) {
        const span = checkedRadio.parentElement.querySelector('span');
        return span ? span.textContent.trim() : checkedRadio.value;
    }
    return undefined;
}
